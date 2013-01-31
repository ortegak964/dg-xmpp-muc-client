# v2.0, now with a nice UI and 1725% more comments stating the obvious!

import '/re/split'
import '/time/strftime'

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


wnd = Gtk.Window!
wnd.add grid where
  # grid :: Grid
  #
  # A container for everything.
  #
  grid = Gtk.Grid!
  grid.attach paned 0 0 1 1 where
    # paned :: Paned
    #
    # A smaller container for read-only stuff.
    #
    paned = Gtk.Paned
      hexpand: True
      vexpand: True

    paned.pack1 frame True True where
      # frame :: Frame
      #
      # A simple decorative frame.
      #
      frame = Gtk.Frame
        margin:      2
        hexpand:     True
        vexpand:     True
        shadow_type: Gtk.ShadowType.ETCHED_IN

      frame.add output where
        # rewind :: Adjustment -> IO ()
        #
        # Scroll to the bottom.
        #
        rewind = a ->
          GObject.idle_add () -> (a.set_value a.get_upper!) if
            max 0 (a.get_upper! - a.get_page_size!) - a.get_value! < 10

        # output :: ScrolledWindow
        #
        # Used to add scrollbars to TextView.
        #
        output = Gtk.ScrolledWindow!

        output.connect 'size-allocate' $ (_ _) -> rewind output.get_vadjustment!
        output.add view where
          # log :: (TextBuffer, str) -> dict str object -> str
          #
          # Given a buffer and a format string, create a logging function.
          #
          log = (buffer fmt) -> delegate q ->
            a = drop 1 $ split r'\{!(\w+)\}' $ strftime fmt
            exhaust $ map (tag text) -> (
              text = text.format_map q
              buffer.insert buffer.get_end_iter! text
              st = buffer.get_end_iter!
              st.backward_chars $ len text
              buffer.apply_tag_by_name tag st buffer.get_end_iter!
            ) a a

          # view :: TextView
          #
          # Where messages go.
          #
          view = Gtk.TextView
            editable:       False
            cursor_visible: False
            wrap_mode:      Gtk.WrapMode.WORD

          buffer = view.get_buffer!
          buffer.create_tag 'time' foreground: '#666666'
          buffer.create_tag 'nick' foreground: '#0066ff' weight: Pango.Weight.BOLD
          buffer.create_tag 'join' foreground: '#119900' weight: Pango.Weight.BOLD
          buffer.create_tag 'quit' foreground: '#991100' weight: Pango.Weight.BOLD
          buffer.create_tag 'text' foreground: '#333333'

          # Note that an empty line above some text looks better than one below.
          message = '{!time}\n%H:%M:%S {!nick}{mucnick} {!text}{body}'
          joined  = '{!time}\n%H:%M:%S +{!join}{muc[nick]}'
          left    = '{!time}\n%H:%M:%S -{!quit}{muc[nick]}'

          self.add_event_handler ('muc::{}::message'.format     room) $ log buffer message
          self.add_event_handler ('muc::{}::got_online'.format  room) $ log buffer joined
          self.add_event_handler ('muc::{}::got_offline'.format room) $ log buffer left

          buffer.connect 'changed' $ _ -> rewind output.get_vadjustment!

    paned.pack2 frame False False where
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
      # A list of all participants.
      #
      model = Gtk.ListStore str str
      model.set_sort_column_id 0 Gtk.SortType.ASCENDING

      # roster :: TreeView
      #
      # The same thing as a widget.
      #
      roster = Gtk.TreeView model
      roster.append_column $ Gtk.TreeViewColumn
        'Participants'
        Gtk.CellRendererText!
        text:       0
        foreground: 1

      # scroll :: ScrolledWindow
      #
      # I don't even expect TreeView to scroll on its own.
      #
      scroll = Gtk.ScrolledWindow!
      scroll.add roster

      # frame :: Frame
      #
      # Another nice frame, yay!
      #
      frame  = Gtk.Frame
        margin:      2
        vexpand:     True
        shadow_type: Gtk.ShadowType.ETCHED_IN

      frame.add scroll
      frame.set_size_request 100 100

      self.add_event_handler ('muc::{}::presence'.format room) $ delegate st ->
        nick     = st !! 'muc' !! 'nick'
        offline  = st !! 'type' == 'unavailable'

        presenceColor = x -> switch
          x == 'dnd'  = '#910'
          x == 'away' = '#f60'
          x == 'chat' = '#190'
          True        = '#333'

        # 1. Remove the old entry.
        exhaust $ map (model !!~) $ findByColumn model 0 nick
        # 2. Add the same entry.
        model.append $ list' nick $ presenceColor (st !! 'type') if not offline
        # 3. ???????
        # 4. False. (Wait, no, `delegate` will return `False` anyway.)

  grid.attach entry 0 1 1 1 where
    # entry :: Entry
    #
    # An editable field for sending messages.
    #
    entry = Gtk.Entry hexpand: True
    entry.connect 'activate' w ->
      self.send_message room w.get_text! mtype: 'groupchat'
      w.set_text ''

wnd.connect 'delete-event' Gtk.main_quit
wnd.connect 'delete-event' (_ _) -> self.disconnect!
wnd.show_all!

GObject.threads_init!
self.process! if self.connect!
Gtk.main!