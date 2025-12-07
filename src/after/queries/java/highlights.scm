;; extends

["permits"] @keyword

(record_declaration 
  name: (identifier) @type) 

(import_declaration 
  (scoped_identifier) @module)

(import_declaration 
  (asterisk) @module) 

(super) @constructor
