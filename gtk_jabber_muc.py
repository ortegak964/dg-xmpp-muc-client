import '/re'
import '/hashlib/md5'
import '/time/strftime'
import '/itertools/cycle'

import '/gi/repository/Gdk'
import '/gi/repository/Gtk'
import '/gi/repository/Pango'
import '/gi/repository/GObject'

import '/sleekxmpp/Message'
import '/sleekxmpp/Presence'
import '/sleekxmpp/ClientXMPP'
import '/sleekxmpp/plugins/xep_0045/XEP_0045'


## Config.

jid      = 'jid@jabber.org'
password = 'password'
rooms    = list' ('room@conference.jabber.org', 'nick')


## Gtk tools and extensions.

Gdk.Pixbuf = import '/gi/repository/GdkPixbuf/Pixbuf'


# at_bottom :: bool
#
# Whether the view is somewhere near the bottom.
#
# NOTE acceptable deviation is 5 pixels.
#
Gtk.Adjustment.at_bottom = property self ->
  self.props.upper - self.props.page_size - self.props.value < 5


# auto_rewind :: IO ()
#
# Let the `Adjustment` automatically scroll to the bottom on update
# if it was there before.
#
Gtk.Adjustment.auto_rewind = property self ->
  x = self.at_bottom
  self.connect 'value-changed' a -> (x = a.at_bottom)
  self.connect       'changed' a -> (a.set_value a.props.upper if x)


# with :: (Widget, *, **) -> Container
#
# 1. Create a widget.
# 2. `add` another widget.
# 3. ???????
# 4. UI!
#
Gtk.Container.with = classmethod (cls other *: a **: k) ->
  self = cls *: a **: k
  self.add other
  self


# scrollable :: (Widget, **) -> Frame
#
# Create a frame with a couple of predefined properties and some scrollbars.
#
Gtk.Frame.scrollable = classmethod (cls other auto_rewind: False **: k) ->
  scroll = Gtk.ScrolledWindow.with other
  scroll.props.vadjustment.auto_rewind if auto_rewind
  cls.with scroll margin: 2 shadow_type: Gtk.ShadowType.ETCHED_IN **: k


# will_stretch :: Widget -> bool
#
# Whether another widget will stretch in the direction of this one.
#
Gtk.Orientable.will_stretch = (self w) ->
  w.props.vexpand if self.props.orientation == Gtk.Orientation.VERTICAL else w.props.hexpand


# with :: (Widget, Widget, *, **) -> Paned
#
# This class requires *two* widgets.
#
Gtk.Paned.with = classmethod (cls p q *: a **: k) ->
  self = cls *: a **: k
  self.pack1 p (self.will_stretch p) False
  self.pack2 q (self.will_stretch q) False
  self


# append :: (str, *) -> IO ()
#
# `insert_with_tags` and `insert_pixbuf` bound to `get_end_iter`.
#
Gtk.TextBuffer.append = (self item *: tags) -> switch
  item :: Gdk.Pixbuf = self.insert_pixbuf    self.get_end_iter! item
  item :: str        = self.insert_with_tags self.get_end_iter! item *: tags
  True = raise $ TypeError $ '{} is not insertable'.format item


# autocomplete :: iter [str] -> IO ()
#
# Make the buffer contain any of the strings in the list that start
# with the buffer's current contents.
#
Gtk.TextBuffer.autocomplete = (self items) ->
  st = self.props.text
  ns = list $ filter x -> (x.startswith st if st) items
  self.append $ ''.join $ drop (len st) $ head ns if ns


# reapply :: (str, [(str, str -> IO ())]) -> IO ()
#
# Split a text by some regex, apply a function to even-numbered parts,
# repeat with the next regex for all other ones.
#
# (I know that explanation sucks. Just look at the `linkify` function below.)
#
reapply = (x ((rx, f), *rest)) -> switch
  rest = exhaust $ map call (cycle (`reapply` rest, f)) (re.split rx x)
  True = f x


# linkify :: (str, *) -> IO ()
#
# `append` that automatically creates clickable links and inserts emoticons.
#
Gtk.TextBuffer.linkify = (self x *: tags) -> reapply x $ list'
  # If some part looks like a link, we wrap it in a tag.
  r'(?i)([a-z][a-z0-9+\.-]*:(?:[,\.?]?[^\s(<>)"\',\.?%]|%[\da-f]{2}|\([^\s(<>)\'"]+\))+)',
    p -> self.append p tag *: tags where
      # FIXME should use system colors.
      tag = self.tag p foreground: '#0011dd' underline: Pango.Underline.SINGLE
      tag.islink = True

  # If it doesn't, but is an emoticon, replace it with a pixbuf.
  '(' + '|'.join (map re.escape self.emotes) + ')',
    p -> self.append item *: tags where
      theme = Gtk.IconTheme.get_default!
      icon  = self.emotes !! p
      item  = (theme.load_icon icon 16 0 if theme.has_icon icon else p)

  # All others are appeneded as-is.
  None, p -> self.append p *: tags


# tag :: (Maybe str, **) -> TextTag
#
# Find a tag by name, create a new one if none found.
#
Gtk.TextBuffer.tag = (self name: None **: k) ->
  # NOTE `a and b or c` <=> `b if a and b else c`.
  not (name is None) and self.props.tag_table.lookup name or self.create_tag name **: k


# get_link_tags_at :: (int, int) -> [TextTag]
#
# `filter ... $ get_tags $ get_iter_at_location $ window_to_buffer_coords`
#
Gtk.TextView.get_link_tags_at = (self x y) -> list
  filter t -> (getattr t 'islink' False)
    Gtk.TextIter.get_tags
      self.get_iter_at_location *:
        self.window_to_buffer_coords Gtk.TextWindowType.WIDGET x y


# linkifyable :: ** -> TextView
#
# Create a `GtkTextView` and handle its signals properly
# to allow clickable links.
#
Gtk.TextView.linkifyable = classmethod $ (cls **: k) ->
  self = cls **: k
  self.cursors = Gdk.Cursor Gdk.CursorType.XTERM, Gdk.Cursor Gdk.CursorType.HAND2

  self.connect 'motion-notify-event' (self ev) ->
    ev.window.set_cursor $ self.cursors !! bool (self.get_link_tags_at ev.x ev.y)

  self.connect 'button-release-event' (self ev) ->
    ev.button == 1 and
      exhaust $ map
        # That should be better than `webbrowser.open`, I think.
        # Is this function equivalent to `xdg-open`?
        t -> Gtk.show_uri ev.window.get_screen! t.props.name 0
        self.get_link_tags_at ev.x ev.y

  # FIXME should also add some items to the popup menu.
  self


# find :: (a, int) -> iter [TreeIter]
#
# Find the first item in a tree model for which the `n`-th column
# contains `x`.
#
Gtk.TreeModel.find = (self x n) ->
  take 1 $ dropwhile i -> (self !! i !! n != x) $ takewhile i -> i
    iterate self.iter_next self.get_iter_first!


# delegate :: (* -> a) -> * -> IO ()
#
# Make the function always run in the main thread. Its result is ignored.
#
delegate = f -> bind GObject.idle_add ((_ -> False) <- f)


## SleekXMPP extensions.

# _ :: a
#
# Aliases for various stanza fields.
#
Message.body    = property $ !! 'body'
Message.nick    = property $ !! 'mucnick'
Message.subject = property $ !! 'subject'
Presence.type   = property $ !! 'type'
Presence.nick   = property $ !! 'nick' <- !! 'muc'


# muc :: XEP_0045
#
# Alias for `self.plugin !! 'xep_0045'`.
#
ClientXMPP.muc = property $ !! 'xep_0045' <- `getattr` 'plugin'


# handle_groupchat_subject :: Message -> IO ()
#
# Patch the original subject handler to also emit `muc::%s::subject` signal.
#
XEP_0045.handle_groupchat_subject = (self msg) ->
  self.xmpp.event 'groupchat_subject' msg
  self.xmpp.event ('muc::{}::subject'.format (msg !! 'from').bare) msg


## XMPP init stuff.

self = ClientXMPP jid password
self.register_plugin 'xep_0030'
self.register_plugin 'xep_0045'
self.register_plugin 'xep_0199'
#self.register_plugin 'xep_0048' $ dict auto_join: True storage_method: 'xep_0223'

self.add_event_handler 'session_start' _ ->
  self.get_roster!
  self.send_presence!
  exhaust $ map (r, n) -> (self.muc.joinMUC r n maxhistory: '20') rooms


## UI init stuff.

make_pane = (room nick) -> Gtk.Paned.with
  orientation: Gtk.Orientation.VERTICAL

  Gtk.Paned.with
    hexpand: True
    vexpand: True

    Gtk.Frame.scrollable view hexpand: True vexpand: True auto_rewind: True where
      # colorify :: str -> str
      #
      # Generate a preudorandom color given a seed string.
      # Luminance of that color is capped at 0.5.
      #
      colorify = x ->
        r, g, b = take 3 $ (md5 $ x.encode 'utf-8').digest!
        m = min 0 (127 - 0.299 * r - 0.587 * g - 0.114 * b)
        '#{:0>2x}{:0>2x}{:0>2x}'.format *: (map (bind max 0 <- round m +) (r, g, b))

      # view :: TextView
      #
      # Where messages go.
      #
      view = Gtk.TextView.linkifyable
        editable:       False
        cursor_visible: False
        wrap_mode:      Gtk.WrapMode.WORD

      buffer = view.props.buffer
      time      = buffer.tag foreground: '#777777'
      highlight = buffer.tag foreground: '#990011'
      text      = buffer.tag foreground: '#333333'
      system    = buffer.tag foreground: '#dd4400' style:  Pango.Style.ITALIC
      ntag = n -> buffer.tag ('n#' + n) foreground: (colorify n) weight: Pango.Weight.BOLD
      mtag = n -> buffer.tag ('m#' + n) foreground: (colorify n) style:  Pango.Style.ITALIC

      buffer.emotes = dict'
        ':)',  'face-smile'
        ':-)', 'face-smile'
        ':D',  'face-laugh'
        ':-D', 'face-laugh'
        ':(',  'face-sad'
        ':-(', 'face-sad'
        ":'(", 'face-crying'
        ':|',  'face-plain'
        ':-|', 'face-plain'
        ':<',  'face-worried'

      joined = x ->
        buffer.append (strftime '\n%H:%M:%S +') time
        buffer.append x.nick $ ntag x.nick

      left = x ->
        buffer.append (strftime '\n%H:%M:%S −') time
        buffer.append x.nick $ ntag x.nick

      message = x ->
        x.time = strftime '\n%H:%M:%S '

        switch
          x.nick == '' =  # Server-issued message.
            buffer.append  x.time time
            buffer.linkify x.body system

          x.body.startswith '/me ' =  # Self-referencing message.
            buffer.append   x.time time
            buffer.append  (x.nick + ' ')           $ mtag x.nick
            buffer.linkify (x.body !! slice 4 None) $ mtag x.nick

          nick in x.body =  # Highlighted message.
            buffer.append x.time highlight
            buffer.append x.nick $ ntag x.nick
            buffer.append ': ' time
            buffer.linkify x.body text

          True =  # Boring stuff you shouldn't read.
            buffer.append  x.time   time
            buffer.append  x.nick $ ntag x.nick
            buffer.append  ': '     time
            buffer.linkify x.body   text

      subject = x ->
        buffer.append (strftime '\n%H:%M:%S ') time
        buffer.append  x.nick system
        buffer.append  ' has set the subject to: ' system
        buffer.linkify x.subject system

      self.add_event_handler ('muc::{}::subject'.format     room) $ delegate subject
      self.add_event_handler ('muc::{}::message'.format     room) $ delegate message
      self.add_event_handler ('muc::{}::got_online'.format  room) $ delegate joined
      self.add_event_handler ('muc::{}::got_offline'.format room) $ delegate left

    Gtk.Frame.scrollable roster vexpand: True where
      # roster :: TreeView
      #
      # A list of all participants.
      #
      roster = Gtk.TreeView model where
        # status :: str -> str
        #
        # Get an icon name given a presence type.
        #
        status = x -> (dict
          dnd:  'empathy-busy'
          away: 'empathy-away'
          xa:   'empathy-extended-away'
        ).get x 'empathy-available'

        # model :: ListStore
        #
        # The same thing as raw data.
        #
        model = Gtk.ListStore str str
        model.set_sort_column_id 0 Gtk.SortType.ASCENDING

        self.add_event_handler ('muc::{}::presence'.format room) $ delegate p ->
          # 1. Remove the old entry.
          exhaust $ map (model !!~) $ model.find p.nick 0
          # 2. Add the same entry.
          #    (Easier than checking if there's already one.)
          model.append (p.nick, status p.type) if p.type != 'unavailable'
          # 3. ???????
          # 4. False. (Wait, no, `delegate` will return `False` anyway.)

      roster.set_headers_visible False
      roster.append_column $ Gtk.TreeViewColumn '' Gtk.CellRendererPixbuf! icon_name: 1
      roster.append_column $ Gtk.TreeViewColumn '' Gtk.CellRendererText!   text: 0

  Gtk.Frame.scrollable entry hexpand: True where
    # send :: TextBuffer -> bool
    #
    # Send a message to the active room, return True.
    #
    send = b -> b.set_text '' where self.send_message room b.props.text mtype: 'groupchat'

    # entry :: Entry
    #
    # An editable field for sending messages.
    #
    entry = Gtk.TextView editable: True wrap_mode: Gtk.WrapMode.WORD
    entry.connect 'key-press-event' $ (w ev) -> switch
      # `Return` send s the message.
      # `Tab` autocompletes a nickname at the start of the message.
      # Using any of these with `Shift` reverts them to their original action.
      ev.state & Gdk.ModifierType.SHIFT_MASK = False
      ev.keyval == Gdk.KEY_Return = True where send w.props.buffer
      ev.keyval == Gdk.KEY_Tab    = True where w.props.buffer.autocomplete
        map (+ ', ') $ self.muc.rooms !! room


wnd = Gtk.Window.with tabs where
  tabs = Gtk.Notebook show_border: False tab_pos: Gtk.PositionType.BOTTOM
  exhaust $ map (r, n) -> (tabs.append_page (make_pane r n) (Gtk.Label r)) rooms

wnd.connect 'delete-event' Gtk.main_quit
wnd.connect 'delete-event' (_ _) -> self.disconnect!
wnd.show_all!

## Profit.

GObject.threads_init!
self.process! if self.connect!
Gtk.main!