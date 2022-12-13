# Changelog

## 3.0.0

### Breaking changes

- Changed updateWithRequest to updateWith to allow more configuration of update.

### New features

- New update options: `onSelectedChanged`, `onInput`, `onFocus` and `onLoseFocus` allow triggering your messages from update functions.
- `withSelectOnTab` enables configuring whether the currently highlighted option is selected on pressing tab.
- `defaultOptionElement` provides a simpler way to change how the text label shows for an option if you want to keep the default option styling.

### Enhancements

- The currently selected item should scroll into view when the input if focused.
- Slightly darkened the selected colour of the default option element.

## 2.1.1

- Added Select.isFocused
