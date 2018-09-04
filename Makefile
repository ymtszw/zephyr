.PHONY: all
all: start

.PHONY: start
start:
	elm-live --pushstate --dir=dist --before-build='./before_build' --output=dist/zephyr.js src/Main.elm

.PHONY: release
release: clean dist/index.html dist/zephyr.js

.PHONY: clean
clean:
	rm -rf elm-stuff/
	rm -rf dist/

dist/index.html:
	@chmod +x before_build
	./before_build

dist/zephyr.js:
	elm make --optimize --output=dist/zephyr.js src/Main.elm
