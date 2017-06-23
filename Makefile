test:
	elm-test ./test/All.elm

local-docs:
	elm-doc . --output docs --exclude 'samples.*,All,*Test'
