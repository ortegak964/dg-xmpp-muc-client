import '/hashlib/md5'
import '/time/strftime'

import '/gi/repository/Gdk'
import '/gi/repository/Gtk'
import '/gi/repository/Pango'

import '/sleekxmpp/Message'
import '/sleekxmpp/Presence'
import '/sleekxmpp/ClientXMPP'

import '/gtktools/delegate'


## Config.

XMPP_USER_ID   = 'jid@example.com'
XMPP_PASSWORD  = 'password'
XMPP_MUC_ROOMS = list'
  'room@conference.example.com', 'nick'


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
Presence.room   = property $ !! 'room' <- !! 'muc'
Presence.error  = property $ !! 'condition' <- !! 'error'


# muc :: XEP_0045
#
# Alias for `self.plugin !! 'xep_0045'`.
#
ClientXMPP.muc = property $ !! 'xep_0045' <- `getattr` 'plugin'


## XMPP init stuff.

self = ClientXMPP XMPP_USER_ID XMPP_PASSWORD
self.register_plugin 'xep_0030'
self.register_plugin 'xep_0045'
self.register_plugin 'xep_0199'
#self.register_plugin 'xep_0048' $ dict auto_join: True storage_method: 'xep_0223'

self.add_event_handler 'session_start' _ ->
  self.get_roster!
  self.send_presence!

self.add_event_handler 'groupchat_subject' m ->
  # FIXME this should probably be sent as a pull request to SleekXMPP.
  self.event ('muc::{}::subject'.format (m !! 'from').bare) m


## UI init stuff.

# make_pane :: str -> Widget
#
# Create a normal room display.
#
make_pane = room -> Gtk.Paned.with
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
            buffer.append   x.time                    time
            buffer.append  (x.nick + ' ')           $ mtag x.nick
            buffer.linkify (x.body !! slice 4 None) $ mtag x.nick

          self.muc.ourNicks !! room in x.body =  # Highlighted message.
            buffer.append  x.time   highlight
            buffer.append  x.nick $ ntag x.nick
            buffer.append  ': '     time
            buffer.linkify x.body   text

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

        model = Gtk.ListStore str str
        model.set_sort_column_id 0 Gtk.SortType.ASCENDING

        self.add_event_handler ('muc::{}::presence'.format room) $ delegate p ->
          # 1. Remove the old entry.
          for (model.find p.nick 0) (model !!~)
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

    entry = Gtk.TextView editable: True wrap_mode: Gtk.WrapMode.WORD
    entry.connect 'key-press-event' $ (w ev) -> switch
      # `Return` sends the message.
      # `Tab` autocompletes a nickname at the start of the message.
      # Using any of these with `Shift` reverts them to their original action.
      ev.state & Gdk.ModifierType.SHIFT_MASK = False
      ev.keyval == Gdk.KEY_Return = True where send w.props.buffer
      ev.keyval == Gdk.KEY_Tab    = True where w.props.buffer.autocomplete
        map (+ ', ') $ self.muc.rooms !! room


# error_message :: Presence -> Widget
#
# Create an exceptional case handling UI.
#
# FIXME it's awful.
#
error_message = p -> Gtk.Grid.padded
  orientation: Gtk.Orientation.VERTICAL

  Gtk.Label hexpand: True $ switch
    p.error == 'conflict'              = 'Nickname {.nick} already taken.'.format p
    p.error == 'forbidden'             = 'Banned.'
    p.error == 'not-allowed'           = "Couldn't create a new room."
    p.error == 'item-not-found'        = 'This room is locked.'
    p.error == 'not-authorized'        = 'Password required.'
    p.error == 'service-unavailable'   = 'This room is overcrowded.'
    p.error == 'registration-required' = 'Not a member of this room.'
    True = 'Unknown error: {.error}.'.format p

  switch
    p.error == 'conflict' = Gtk.Grid.padded entry retry vexpand: False where
      entry = Gtk.Entry text: (p.nick + "'")
      retry = Gtk.Button 'Try again'

      retry_f = _ ->
        entry.set_sensitive False
        retry.set_sensitive False
        self.muc.joinMUC p.room entry.props.text maxhistory: '20'

      entry.connect 'activate' retry_f
      retry.connect 'clicked'  retry_f

    p.error == 'not-authorized' = Gtk.Grid.padded entry retry vexpand: False where
      entry = Gtk.Entry visibility: False placeholder_text: 'Know one?'
      retry = Gtk.Button 'Try again'

      retry_f = _ ->
        entry.set_sensitive False
        retry.set_sensitive False
        self.muc.joinMUC p.room p.nick password: entry.props.text maxhistory: '20'

      entry.connect 'activate' retry_f
      retry.connect 'clicked'  retry_f

    True = Gtk.Grid.padded retry vexpand: False where
      retry = Gtk.Button 'Try again'
      retry.connect 'clicked' _ ->
        retry.set_sensitive False
        self.muc.joinMUC p.room p.nick maxhistory: '20'


wnd = Gtk.Window.with tabs where
  tabs = Gtk.Notebook show_border: False tab_pos: Gtk.PositionType.BOTTOM
  nums = dict!
  wgts = dict!

  self.add_event_handler 'session_start' $ delegate $ _ -> for XMPP_MUC_ROOMS (r, n) ->
    # NOTE `make_pane` should be called once per room, then reused.
    #  Attempting to remove it will result in a memory leak since
    #  `self` keeps references to event handlers and all their closures.
    #
    #  Also, it should be created *before* calling `joinMUC` as
    #  it connects to various signals that may otherwise be ignored.
    #  It's never a good thing when a half of the roster is missing.
    wgts !! r = make_pane r
    nums !! r = tabs.append_page (wgts !! r) (Gtk.Label r)
    self.muc.joinMUC r n maxhistory: '20'

  self.add_event_handler 'groupchat_presence' $ delegate $ p -> switch
    self.muc.ourNicks !! p.room != p.nick = None
    p.type == 'error' = tabs.replace_page (nums !! p.room) $ error_message p
    p.type != 'error' = tabs.replace_page (nums !! p.room) $ wgts !! p.room

wnd.connect 'delete-event' Gtk.main_quit
wnd.connect 'delete-event' (_ _) -> self.disconnect!
wnd.show_all!

## Profit.

self.process! if self.connect!
Gtk.main!