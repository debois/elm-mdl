ELM=elm-make --yes
PAGES=../elm-mdl-gh-pages
CLOSURE_COMPILER=compiler.jar

demo:
	(cd demo; $(ELM) Demo.elm --warn --output ../elm.js)

run-demo:
	(cd demo; pkill elm-reactor; elm-reactor &)

compile : demo
	java -jar $(CLOSURE_COMPILER) -O ADVANCED --assume_function_wrapper --js elm.js > /tmp/elm.js && mv /tmp/elm.js elm.js
	

counter1: 
	$(ELM) examples/Counter.elm --warn --output examples/counter1.html 
	
counter-many: 
	$(ELM) examples/Counter-many.elm --warn --output examples/counter-many.html

counter-no-shorthand: 
	$(ELM) examples/Counter-no-shorthand.elm --warn --output examples/counter-no-shorthand.html

docs: 
	$(ELM) --docs=docs.json 

test: docs demo counter1 counter-many counter-no-shorthand

copy-assets : 
	(cd demo; cp -r assets ../$(PAGES))
	(cd $(PAGES); git add assets)
	
wip-pages : 
	(cd demo; elm-make Demo.elm --output ../$(PAGES)/wip.js)
	(cd $(PAGES); git commit -am "Update."; git push origin gh-pages)

pages : 
	(cd demo; elm-make Demo.elm --output ../$(PAGES)/elm.js)
	(cd $(PAGES); git commit -am "Update."; git push origin gh-pages)

# Note: requires https://github.com/google/material-design-lite in
# ./material-design-lite
css: styles/select.scss
	sassc styles/select.scss > styles/select.css

cleanish :
	rm -f elm.js index.html docs.json

clean : cleanish
	rm -rf elm-stuff/build-artifacts demo/elm-stuff/build-artifacts

distclean : clean
	rm -rf elm-stuff demo/elm-stuff


install-hooks: 
	cp build_scripts/hooks/commit-msg .git/hooks
	chmod a+x .git/hooks/commit-msg


.PHONY : pages elm.js clean cleanish distclean demo docs test copy-assets install-hooks
