.PHONY: all
all: start

.PHONY: start
start:
	elm-live --pushstate --dir=docs --output=docs/zephyr.js src/Main.elm

.PHONY: release
release: clean docs/zephyr.js

.PHONY: clean
clean:
	rm -rf elm-stuff/
	rm -f docs/zephyr.js

docs/zephyr.js:
	elm make --optimize --output=docs/zephyr.js src/Main.elm
