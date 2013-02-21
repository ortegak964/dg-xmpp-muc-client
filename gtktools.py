import '/re'
import '/itertools/cycle'

import '/gi/repository/Gtk'
import '/gi/repository/Gdk'
import '/gi/repository/Pango'
import '/gi/repository/GObject'

import '/gi/repository/GdkPixbuf/Pixbuf'


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


# with :: (*, **) -> Container
#
# 1. Create a widget.
# 2. `add` some more widgets.
# 3. ???????
# 4. UI!
#
Gtk.Container.with = classmethod (cls *: others **: k) ->
  self = cls **: k
  for others self.add
  self


# padded :: (*, **) -> Container
#
# Like `with`, but adds two expandable `Label`s at the ends.
#
Gtk.Container.padded = classmethod (cls *: others **: k) ->
  self = cls.with (Gtk.Label hexpand: True vexpand: True) *: others **: k
  self.add        (Gtk.Label hexpand: True vexpand: True)
  self


# scrollable :: (Widget, **) -> Frame
#
# Create a frame with a couple of predefined properties and some scrollbars.
#
Gtk.Frame.scrollable = classmethod (cls other auto_rewind: False **: k) ->
  scroll = Gtk.ScrolledWindow.with other
  scroll.props.vadjustment.auto_rewind if auto_rewind
  cls.with scroll margin: 2 shadow_type: Gtk.ShadowType.ETCHED_IN **: k


# replace_page :: (int, Widget) -> ()
#
# Replace the widget on the n-th page of a notebook.
#
Gtk.Notebook.replace_page = (self n new) ->
  move  = self.get_current_page! == n
  label = self.get_tab_label $ self.get_nth_page n
  self.remove_page n
  self.insert_page new label n
  new.show_all!
  self.set_current_page n if move


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
  item :: Pixbuf = self.insert_pixbuf    self.get_end_iter! item
  item :: str    = self.insert_with_tags self.get_end_iter! item *: tags
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
    ev.button == 1 and for (self.get_link_tags_at ev.x ev.y) t ->
      # That should be better than `webbrowser.open`, I think.
      # Is this function equivalent to `xdg-open`?
      Gtk.show_uri ev.window.get_screen! t.props.name 0

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

GObject.threads_init!