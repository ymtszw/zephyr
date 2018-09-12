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
	elm make --optimize --output=dist/zephyr-optimized.js src/Main.elm
	@# Taken from https://github.com/rtfeldman/elm-spa-example/blob/master/README.md
	uglifyjs dist/zephyr-optimized.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=dist/zephyr-compressed.js && rm -v dist/zephyr-optimized.js
	uglifyjs dist/zephyr-compressed.js --mangle --output=dist/zephyr.js && rm -v dist/zephyr-compressed.js
