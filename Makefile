.PHONY: all
all: start

.PHONY: start
start:
	elm-live --pushstate --dir=dist --before-build='./before_build' --output=dist/zephyr.js src/Main.elm

.PHONY: release
release: clean before_build dist/zephyr.js after_build

.PHONY: clean
clean:
	rm -rf elm-stuff/
	rm -rf dist/

.PHONY: before_build
before_build:
	bash ./before_build

dist/zephyr.js:
	elm make --optimize --output=dist/zephyr.js src/Main.elm

.PHONY: after_build
after_build:
	sed -i '/livereload/d' dist/index.html
