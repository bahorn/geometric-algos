.PHONY: test

build:
	stack build

run:
	stack run

test:
	stack test

ghcid:
	ghcid --color=always -l="hlint --color=always ."  -c "stack ghci"

ghcid_test:
	ghcid --target test --color=always -l="hlint --color=always ."  -c "stack ghci" 

docker:
	docker build -t bahorn/wlp .

docker-run:
	docker run -ti bahorn/wlp stack run
