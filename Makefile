.PHONY: test local-docs

test:
	elm-test ./test/All.elm

docs-json:
	elm-make --docs=documentation.json

local-docs:
	elm-doc . --output docs --exclude 'samples.*,All,*Test,Utils'
