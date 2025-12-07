;; extends

((function_call
   name: (dot_index_expression
		   table: (dot_index_expression
					table: (dot_index_expression
							 table: (identifier) @vim
							 field: (identifier) @treesitter)
					field: (identifier) @query)
		   field: (identifier) @parse)
   arguments: (arguments
				(string
				  content: _ @injection.content) .))
 (#set! injection.language "query")
 (#eq? @vim "vim")
 (#eq? @treesitter "treesitter")
 (#eq? @query "query")
 (#any-of? @parse "parse" "set"))
