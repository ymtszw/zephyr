.PHONY: all
all: start

.PHONY: start
start:
	elm-live src/Main.elm --pushstate --dir=dist --before-build='./before_build' -- --output=dist/zephyr.js

.PHONY: build
build:
	elm make src/Main.elm --output=dist/zephyr.js --debug

.PHONY: release
release: clean before_build dist/zephyr.js

.PHONY: clean
clean:
	rm -rf elm-stuff/
	rm -rf dist/
	rm -f tests/ElmjutsuDumMyM0DuL3.elm

.PHONY: before_build
before_build:
	bash ./before_build

dist/zephyr.js:
	elm make --optimize --output=dist/zephyr-optimized.js src/Main.elm
	google-closure-compiler --js=dist/zephyr-optimized.js --js_output_file=dist/zephyr.js && rm -v dist/zephyr-optimized.js
	du -h dist/zephyr.js

.PHONY: lab
lab:
	elm-live src/View/PatternLab.elm --pushstate --port=8001 --start-page=index.html -- --output=index.html

.PHONY: analyse
analyse:
	elm-analyse --serve --port=3000 --open
