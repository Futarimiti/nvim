;; extends

(comment) @comment

(command
  (command_name) @keyword)

(command
  (selector) @special)

(execute_keyword) @keyword

(number) @number

(namespace) @module

(text) @string

(location
  (coordinate
    [ "~" ] @punctuation.special))

(nbt_object_key
  (string) @string)

(nbt_object_value
  (string) @string)

(nbt_object_value
  (boolean) @boolean)

(selector
  [ "@" ] @punctuation.special)

(identifier) @variable
