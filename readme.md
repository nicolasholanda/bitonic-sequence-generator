## About
This project is based on the [article about generating bitonic sequences](https://www.geeksforgeeks.org/dsa/generate-bitonic-sequence-of-length-n-from-integers-in-a-given-range/), 


## What is a bitonic sequence
It's a numeric sequency where the first case grows and degrees from a max point or from the min point. In short it have a max and min point "Example: {1, 5, 6, 9, 8, 7, 3, 0}"

## The problem
With at least 3 integers (N, L, R) we need to generate a bitonic sequency using N numbers on the interval (L,R) in a way where the sequency strict grows and then decrease, the first element of the seuqency must be the max possible. If not possible to build the sequency it must return -1.

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