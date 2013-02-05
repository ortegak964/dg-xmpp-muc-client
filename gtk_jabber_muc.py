## Config.

jid      = 'jid@jabber.org'
password = 'password'
room     = 'room@conference.jabber.org'
nick     = 'nick'


## Misc stuff.

import '/re'
import '/webbrowser'
import '/itertools/cycle'


# linkify :: str -> [(str, bool)]
#
# Split some text into a sequence of pairs `A`, where
#   fst A == part of that text
#   snd A == whether that part is a hyperlink
#
link_re = re.compile $ r'([a-z][a-z0-9+\.-]*:(?:[,\.?]?[^\s(<>)"' + "',\.?%]|%\d{2}|\([^\s(<>)'" + r'"]+\))+)'
linkify = x -> zip (link_re.split x) $ cycle (False, True)

import '/hashlib/md5'
import '/time/strftime'


# colorify :: str -> str
#
# Generate a preudorandom color given a seed string.
# Luminance of the resulting color is capped at 0.5.
#
colorify = x ->
  r, g, b = take 3 $ (md5 $ x.encode 'utf-8').digest!
  m = min 0 (127 - 0.299 * r - 0.587 * g - 0.114 * b)
  '#{:0>2x}{:0>2x}{:0>2x}'.format *: (map (+ round m) (r, g, b))


## Gtk tools and extensions.

import '/gi/repository/Gdk'
import '/gi/repository/Gtk'
import '/gi/repository/Pango'
import '/gi/repository/GObject'


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


# append_with_tags :: (str, *) -> IO ()
#
# `insert_with_tags` bound to `get_end_iter`.
#
Gtk.TextBuffer.append_with_tags = (self text *: tags) ->
  self.insert_with_tags self.get_end_iter! text *: tags


# linkify_with_tags :: (str, *) -> IO ()
#
# `append_with_tags` that automatically creates clickable links.
#
Gtk.TextBuffer.linkify_with_tags = (self text *: tags) -> exhaust $ map
  (part, islink) -> switch
    islink =
      tag = self.create_tag None foreground: '#0011dd' underline: Pango.Underline.SINGLE
      tag.connect 'event' (this view ev it) ->
        webbrowser.open part if ev.type == Gdk.EventType.BUTTON_RELEASE and
                            not self.get_has_selection!
        # We don't want the TextView to think there's a selection.
        False

      self.append_with_tags part tag *: tags
    True = self.append_with_tags part *: tags
  linkify text
  

# get_tag :: (str, **) -> TextTag
#
# Find a tag by name, create a new one if none found.
#
Gtk.TextBuffer.get_tag = (self name **: k) ->
  table = self.props.tag_table
  table.lookup name or self.create_tag name **: k


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

import '/sleekxmpp/Message'
import '/sleekxmpp/Presence'
import '/sleekxmpp/ClientXMPP'


Message.body = property $ !! 'body'
Message.nick = property $ !! 'mucnick'

Presence.type = property $ !! 'type'
Presence.nick = property $ !! 'nick' <- !! 'muc'

ClientXMPP.muc = property $ !! 'xep_0045' <- `getattr` 'plugin'


## XMPP init stuff.

self = ClientXMPP jid password
self.register_plugin 'xep_0030'
self.register_plugin 'xep_0045'
self.register_plugin 'xep_0199'

self.add_event_handler 'session_start' _ ->
  self.get_roster!
  self.send_presence!
  self.muc.joinMUC room nick maxhistory: '20' wait: True


## UI init stuff.

wnd = Gtk.Window.with $ Gtk.Paned.with
  orientation: Gtk.Orientation.VERTICAL

  Gtk.Paned.with
    hexpand: True
    vexpand: True

    Gtk.Frame.scrollable view hexpand: True vexpand: True auto_rewind: True where
      # view :: TextView
      #
      # Where messages go.
      #
      view = Gtk.TextView
        editable:       False
        cursor_visible: False
        wrap_mode:      Gtk.WrapMode.WORD

      buffer = view.props.buffer
      time      = buffer.create_tag None foreground: '#777777'
      text      = buffer.create_tag None foreground: '#333333'
      highlight = buffer.create_tag None foreground: '#990011'
      joined_t  = buffer.create_tag None foreground: '#119900' weight: Pango.Weight.BOLD
      left_t    = buffer.create_tag None foreground: '#991100' weight: Pango.Weight.BOLD
      system    = buffer.create_tag None foreground: '#dd4400' style: Pango.Style.ITALIC

      joined = x ->
        buffer.append_with_tags (strftime '\n%H:%M:%S +') time
        buffer.append_with_tags x.nick joined_t

      left = x ->
        buffer.append_with_tags (strftime '\n%H:%M:%S −') time
        buffer.append_with_tags x.nick left_t

      message = x ->
        ntag = buffer.get_tag ('n#' + x.nick) foreground: (colorify x.nick) weight: Pango.Weight.BOLD
        mtag = buffer.get_tag ('n#' + x.nick) foreground: (colorify x.nick) style: Pango.Style.ITALIC

        sysmsg  = x.nick == ''
        selfref = x.body.startswith '/me '
        thisref = nick in x.body

        buffer.append_with_tags (strftime '\n%H:%M:%S ') time

        switch
          sysmsg =
            buffer.linkify_with_tags x.body system
          selfref =
            buffer.append_with_tags (x.nick + ' ') mtag
            buffer.linkify_with_tags (x.body !! slice 4 None) mtag
          True =
            buffer.append_with_tags (x.nick + ' ') ntag
            buffer.linkify_with_tags x.body (highlight if thisref else text)

      self.add_event_handler ('muc::{}::message'.format     room) $ delegate message
      self.add_event_handler ('muc::{}::got_online'.format  room) $ delegate joined
      self.add_event_handler ('muc::{}::got_offline'.format room) $ delegate left

    Gtk.Frame.scrollable roster vexpand: True where
      # roster :: TreeView
      #
      # A list of all participants.
      #
      roster = Gtk.TreeView model where
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
          p.type != 'unavailable' and
            model.append (p.nick, (dict dnd: '#910' away: '#f60' chat: '#190').get p.type '#333')
          # 3. ???????
          # 4. False. (Wait, no, `delegate` will return `False` anyway.)

      roster.append_column $ Gtk.TreeViewColumn 'Participants'
        Gtk.CellRendererText!
        text:       0
        foreground: 1

  Gtk.Frame.scrollable entry hexpand: True where
    # entry :: Entry
    #
    # An editable field for sending messages.
    #
    entry = Gtk.TextView editable: True
    entry.connect 'key-press-event' (w ev) ->
      # Use `Shift+Enter` or `Numpad Enter` to insert a newline.
      ev.keyval == Gdk.KEY_Return and not (ev.state & Gdk.ModifierType.SHIFT_MASK) and
        self.send_message room w.props.buffer.props.text mtype: 'groupchat'
        w.props.buffer.props.text = ''
        True

wnd.connect 'delete-event' Gtk.main_quit
wnd.connect 'delete-event' (_ _) -> self.disconnect!
wnd.show_all!


## Profit.

GObject.threads_init!
self.process! if self.connect!
Gtk.main!