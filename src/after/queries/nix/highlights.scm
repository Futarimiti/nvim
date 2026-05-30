;; extends

;; highlight flake input url
(source_code
  expression: (attrset_expression
    (binding_set
      binding: (binding
        attrpath: (attrpath
          attr: (identifier) @_inputs
          (#eq? @_inputs "inputs"))
        expression: (attrset_expression
          (binding_set
            binding: [
              (binding
                attrpath: (attrpath
                  attr: (identifier)
                  attr: (identifier) @_url
                  (#eq? @_url "url"))
                expression: (string_expression
                  (string_fragment) @string.special.path))
              (binding
                expression: (attrset_expression
                  (binding_set
                    binding: (binding
                      attrpath: (attrpath
                        attr: (identifier) @_url
                        (#eq? @_url "url"))
                      expression: (string_expression
                        (string_fragment) @string.special.path)))))
            ]))))))
