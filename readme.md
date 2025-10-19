## About
This project is based on the [article about generating bitonic sequences](https://www.geeksforgeeks.org/dsa/generate-bitonic-sequence-of-length-n-from-integers-in-a-given-range/), 


## What is a bitonic sequence
It's a numeric sequence where the first case grows and decreases from a max point or from the min point. In short it has a max and min point "Example: {1, 5, 6, 9, 8, 7, 3, 0}"

## The problem
With at least 3 integers (N, L, R) we need to generate a bitonic sequence using N numbers on the interval (L,R) in a way where the sequence strictly grows and then decreases, the first element of the sequence must be the max possible. If not possible to build the sequence it must return -1.

## Build and Run

Start Redis:
```bash
docker-compose up -d
```

Build the project:
```bash
cabal build
```

Run the API:
```bash
cabal run bitonic-api
```

Run tests:
```bash
cabal test
```

### Testing

- Run all tests:
	```bash
	cabal test
	```

- Run an individual test-suite (useful while developing):
	```bash
	cabal test bitonic-test                 # pure unit tests for BitonicSequence
	cabal test bitonic-service-test         # service tests (uses Redis cache)
	cabal test bitonic-repository-test      # repository tests (reads/writes Redis)
	cabal test bitonic-api-integration-test # in-memory API integration tests (Redis)
	```

Notes:
- Redis-backed tests (service/repository/API) expect a Redis instance running. You can start it with:
	```bash
	docker-compose up -d
	```
- If Redis is not available, those tests will be skipped automatically so the rest of the suite can still run.

## Usage

POST to `http://localhost:3000/bitonic` with JSON body:
```json
{"n": 5, "l": 3, "r": 10}
```

Response:
```json
{"request":{"n":5,"l":3,"r":10},"result":[9,10,9,8,7]}
```

## Sources: 

https://kuniga.wordpress.com/2010/12/17/ordenacao-bitonica/
https://www.cin.ufpe.br/~joa/menu_options/school/cursos/ppd/Projetos/BitonicMergeSortJonasSergio/BitonicMergeSort_Scientific.pdf