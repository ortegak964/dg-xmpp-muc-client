# v2.0, now with a nice UI and 1725% more comments stating the obvious!

import '/time/strftime'
import '/hashlib/md5'

import '/gi/repository/Gdk'
import '/gi/repository/Gtk'
import '/gi/repository/Pango'
import '/gi/repository/GObject'

import '/sleekxmpp/ClientXMPP'


jid      = 'jid@jabber.org'
password = 'password'
room     = 'room@conference.jabber.org'
nick     = 'nick'


self = ClientXMPP jid password
self.register_plugin 'xep_0030'
self.register_plugin 'xep_0045'
self.register_plugin 'xep_0199'

self.add_event_handler 'session_start' _ ->
  self.get_roster!
  self.send_presence!
  (self.plugin !! 'xep_0045').joinMUC room nick maxhistory: '20' wait: True


# delegate :: (* -> a) -> * -> IO ()
#
# Make the function always run in the main thread. Its result is ignored.
#
delegate = f -> bind GObject.idle_add ((_ -> False) <- f)


# frameWidget :: (Widget, **) -> (Frame, ScrolledWindow)
#
# Wrap a widget in a frame supplied with scrollbars.
#
frameWidget = (w **: k) ->
  scroll = Gtk.ScrolledWindow!
  scroll.add w

  frame = Gtk.Frame margin: 2 shadow_type: Gtk.ShadowType.ETCHED_IN **: k
  frame.add scroll
  frame, scroll


# paneWidget :: (Widget, Widget, **) -> Paned
#
# Same thing with GtkPaned.
#
paneWidget = (w1 w2 **: k) ->
  # getExpand :: (Widget, Orientable) -> bool
  #
  # Find out whether the former widget expands in the direction of the latter.
  #
  getExpand = (w p) ->
    w.get_vexpand! if p.get_orientation! == Gtk.Orientation.VERTICAL else w.get_hexpand!

  paned = Gtk.Paned **: k
  paned.pack1 w1 (getExpand w1 paned) False
  paned.pack2 w2 (getExpand w2 paned) False
  paned


wnd = Gtk.Window!
wnd.add $ paneWidget
  orientation: Gtk.Orientation.VERTICAL

  paneWidget
    hexpand: True
    vexpand: True

    frame where
      # rewind :: Adjustment -> IO ()
      #
      # Scroll to the bottom.
      #
      rewind = a ->
        GObject.idle_add () -> (a.set_value a.get_upper!) if
          max 0 (a.get_upper! - a.get_page_size!) - a.get_value! < 10

      # log :: (TextBuffer, (Stanza, (str, TextTag) -> ()) -> IO ()) -> Stanza -> IO ()
      #
      # Given a buffer and a formatter, create a logging function.
      #
      log = (buffer fmt) -> bind (delegate fmt) (a b) -> (buffer.insert_with_tags_by_name buffer.get_end_iter! a b)

      # frame  :: Frame
      # output :: ScrolledWindow
      #
      # A simple decorative frame.
      #
      frame, output = frameWidget
        hexpand: True
        vexpand: True

        # view :: TextView
        #
        # Where messages go.
        #
        view = Gtk.TextView
          editable:       False
          cursor_visible: False
          wrap_mode:      Gtk.WrapMode.WORD

      buffer = view.get_buffer!
      buffer.connect 'changed'       $  _    -> rewind output.get_vadjustment!
      output.connect 'size-allocate' $ (_ _) -> rewind output.get_vadjustment!

      buffer.create_tag 'time'      foreground: '#777777'
      buffer.create_tag 'text'      foreground: '#333333'
      buffer.create_tag 'highlight' foreground: '#990011'
      buffer.create_tag 'joined'    foreground: '#119900' weight: Pango.Weight.BOLD
      buffer.create_tag 'left'      foreground: '#991100' weight: Pango.Weight.BOLD

      # tagFor :: str -> str
      #
      # Create a new tag to wrap a string, return its name.
      #
      # (The tag will make the text bold and pseudorandomly colored.)
      #
      tagFor = x ->
        r, g, b = take 3 $ (md5 $ x.encode 'utf-8').digest!
        m = 127 - max 127 (0.299 * r + 0.587 * g + 0.114 * b)
        color = '#{:0>2x}{:0>2x}{:0>2x}'.format *: (map (+ round m) (r, g, b))

        table = buffer.get_tag_table!
        table.lookup ('n#' + x) or
          buffer.create_tag ('n#' + x) foreground: color weight: Pango.Weight.BOLD
        'n#' + x

      # Note that an empty line above some text looks better than one below.
      message = (f x) -> (f (strftime '\n%H:%M:%S ')  'time', f (x !! 'mucnick') (tagFor $ x !! 'mucnick'), f (' ' + x !! 'body') ('m#' if '' == x !! 'mucnick' else 'highlight' if nick in x !! 'body' else 'text'))
      joined  = (f x) -> (f (strftime '\n%H:%M:%S +') 'time', f (x !! 'muc' !! 'nick') 'joined')
      left    = (f x) -> (f (strftime '\n%H:%M:%S -') 'time', f (x !! 'muc' !! 'nick') 'left')

      self.add_event_handler ('muc::{}::message'.format     room) $ log buffer message
      self.add_event_handler ('muc::{}::got_online'.format  room) $ log buffer joined
      self.add_event_handler ('muc::{}::got_offline'.format room) $ log buffer left

      # There is also a system channel without a nickname.
      buffer.create_tag 'm#' foreground: '#dd4400' style: Pango.Style.ITALIC

    frame where
      # frame :: Frame
      #
      # Another nice frame, yay!
      #
      frame = fst $ frameWidget roster vexpand: True where
        # roster :: TreeView
        #
        # A list of all participants.
        #
        roster = Gtk.TreeView model where
          # findByColumn :: (TreeModel, int, a) -> iter [TreeIter]
          #
          # Find the first item in a tree model for which the `n`-th column
          # contains `x`.
          #
          findByColumn = (model n x) ->
            take 1 $ dropwhile i -> (model !! i !! n != x) $ takewhile i -> i
              iterate model.iter_next model.get_iter_first!

          # model :: ListStore
          #
          # The same thing as raw data.
          #
          model = Gtk.ListStore str str
          model.set_sort_column_id 0 Gtk.SortType.ASCENDING

          self.add_event_handler ('muc::{}::presence'.format room) $ delegate st ->
            t    = st !! 'type'
            nick = st !! 'muc' !! 'nick'

            # 1. Remove the old entry.
            exhaust $ map (model !!~) $ findByColumn model 0 nick
            # 2. Add the same entry.
            #    (Easier than checking if there's already one.)
            t != 'unavailable' and
              model.append (nick, (dict dnd: '#910' away: '#f60' chat: '#190').get t '#333')
            # 3. ???????
            # 4. False. (Wait, no, `delegate` will return `False` anyway.)

        roster.append_column $ Gtk.TreeViewColumn 'Participants'
          Gtk.CellRendererText!
          text:       0
          foreground: 1

      frame.set_size_request 100 100

  fst $ frameWidget entry hexpand: True where
    # entry :: Entry
    #
    # An editable field for sending messages.
    #
    entry = Gtk.TextView editable: True
    entry.connect 'key-press-event' (w ev) ->
      # Use `Shift+Enter` or `Numpad Enter` to insert a newline.
      ev.keyval == Gdk.KEY_Return and not (ev.state & Gdk.ModifierType.SHIFT_MASK) and
        b = w.get_buffer!
        t = b.get_text b.get_start_iter! b.get_end_iter! False
        self.send_message room t mtype: 'groupchat'
        b.set_text ''
        True

wnd.connect 'delete-event' Gtk.main_quit
wnd.connect 'delete-event' (_ _) -> self.disconnect!
wnd.show_all!

GObject.threads_init!
self.process! if self.connect!
Gtk.main!