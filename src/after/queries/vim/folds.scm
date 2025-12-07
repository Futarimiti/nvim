;; extends

[
  (map_statement)
  (command_statement)
  (lua_statement)
  (comment)+
  (dictionnary) ; not my fault
] @fold

;; TODO
(augroup_statement
  (augroup_name) @name
  (#not-eq? @name "END")) @fold.start

(augroup_statement
  (augroup_name) @name
  (#eq? @name "END")) @fold.end
