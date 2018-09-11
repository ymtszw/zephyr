.PHONY: all
all: start

.PHONY: start
start:
	elm-live src/Main.elm --pushstate --dir=dist --before-build='./before_build' -- --output=dist/zephyr.js

.PHONY: release
release: clean before_build dist/zephyr.js

.PHONY: clean
clean:
	rm -rf elm-stuff/
	rm -rf dist/

.PHONY: before_build
before_build:
	bash ./before_build

dist/zephyr.js:
	elm make --optimize --output=dist/zephyr.js src/Main.elm
