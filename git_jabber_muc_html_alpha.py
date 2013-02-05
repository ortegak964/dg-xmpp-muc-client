# Changelog:
#
#   1.0:
#
#     * Initial release as a 57 SLOC jabber bot
#
#   2.0:
#
#     * Nice UI
#     * Lots of comments stating the obvious
#
#   3.0:
#
#     * Monkey-patch EVERYTHING!
#
#   4.0:
#
#     * Alpha version of HTML output
# 

import '/re/sub'
import '/hashlib/md5'
import '/time/strftime'
import '/xml/sax/saxutils/escape'

## Config.

jid      = 'jid@jabber.org'
password = 'password'
room     = 'room@conference.jabber.org'
nick     = 'nick'


## Gtk tools and extensions.

import '/gi/repository/Gdk'
import '/gi/repository/Gtk'
import '/gi/repository/Pango'
import '/gi/repository/GObject'
import '/gi/repository/WebKit'


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


# linkify :: str -> str
#
# XML-escape a string and make links clickable.
#
linkify = x ->
  r = r'([a-z][a-z0-9+\.-]*:(?:[,\.?]?[^\s(<>)"' + r"'" + r',\.?%]|%\d{2}|\([^\s(<>)"' + r"'" + r']+\))+)'
  sub r r'<a href="\1" target="_blank">\1</a>' $ escape x


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
      # colorHash :: str -> str
      #
      # Create a pseudorandom color given a seed string.
      #
      colorHash = x ->
        r, g, b = take 3 $ (md5 $ x.encode 'utf-8').digest!
        m = min 0 (127 - 0.299 * r - 0.587 * g - 0.114 * b)
        '#{:0>2x}{:0>2x}{:0>2x}'.format *: (map (+ round m) (r, g, b))

      view = WebKit.WebView editable: False settings: (WebKit.WebSettings
        default_font_family: 'Roboto'
        default_font_size:   10
      )

      dom  = view.get_dom_document!
      body = dom.get_first_child!.get_last_child!

      message         = '<span style="color: #777">%H:%M:%S</span> <span style="color: {0}; font-weight: bold">{1.nick}</span> <span style="color: #333">{1.body}</span>'
      message_thisref = '<span style="color: #777">%H:%M:%S</span> <span style="color: {0}; font-weight: bold">{1.nick}</span> <span style="color: #901">{1.body}</span>'
      message_selfref = '<span style="color: #777">%H:%M:%S</span> <span style="color: {0}; font-style: italic">{1.nick} {1.body}</span>'
      message_system  = '<span style="color: #777">%H:%M:%S</span> <span style="color: #d40; font-style: italic">{1.body}</span>'
      joined          = '<span style="color: #777">%H:%M:%S +</span><span style="color: #190; font-weight: bold">{1.nick}</span>'
      left            = '<span style="color: #777">%H:%M:%S −</span><span style="color: #910; font-weight: bold">{1.nick}</span>'

      format = (t x) ->
        e = dom.create_element 'div'
        e.set_inner_html $ (strftime t).format (colorHash x.nick) x
        body.append_child e

      left_f    = delegate $ x ->
        x !! 'muc' !! 'nick' = escape x.nick
        format left   x
      joined_f  = delegate $ x ->
        x !! 'muc' !! 'nick' = escape x.nick
        format joined x
      message_f = delegate $ x ->
       x !! 'body'    = linkify x.body
       x !! 'mucnick' = escape x.nick
       switch
        x.nick == ''             = format message_system  x
        x.body.startswith '/me ' = format message_selfref x where x !! 'body' = x.body !! slice 4 None
        nick in x.body           = format message_thisref x
        True                     = format message         x

      self.add_event_handler ('muc::{}::message'.format     room) message_f
      self.add_event_handler ('muc::{}::got_online'.format  room) joined_f
      self.add_event_handler ('muc::{}::got_offline'.format room) left_f

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