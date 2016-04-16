ELM=elm-make --yes
PAGES=gh-pages

demo:
	(cd demo; $(ELM) Demo.elm --output ../elm.js)

comp: 
	$(ELM) examples/Component.elm --warn --output elm.js
	
comp-tea: 
	$(ELM) examples/Component.elm --warn --output elm.js

docs: 
	$(ELM) --docs=docs.json 

test: docs comp comp-tea docs
	
wip-pages : 
	(cd demo; elm-make Demo.elm --output ../$(PAGES)/wip.js)
	(cd $(PAGES); git commit -am "Update."; git push origin gh-pages)

pages : 
	(cd demo; elm-make Demo.elm --output ../$(PAGES)/elm.js)
	(cd $(PAGES); git commit -am "Update."; git push origin gh-pages)

cleanish :
	rm -f elm.js index.html docs.json

clean : cleanish
	rm -rf elm-stuff/build-artifacts demo/elm-stuff/build-artifacts

distclean : clean
	rm -rf elm-stuff


.PHONY : pages elm.js clean cleanish distclean demo docs test
