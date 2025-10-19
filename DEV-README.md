# Developer Guide - Bitonic Sequence Generator

> **Welcome!** This guide is designed for developers who are **new to Haskell**. We'll help you understand the codebase step by step.

## 📚 Table of Contents

- [Getting Started](#getting-started)
- [Understanding the Codebase](#understanding-the-codebase)
- [Reading Order](#reading-order)
- [Architecture Overview](#architecture-overview)
- [Common Patterns Explained](#common-patterns-explained)
- [Debugging Tips](#debugging-tips)
- [Useful Commands](#useful-commands)
- [Additional Resources](#additional-resources)

---

## 🚀 Getting Started

### Prerequisites

Before diving into the code, make sure you have:

1. **GHC** (Glasgow Haskell Compiler) installed - version 9.6.7 or higher
2. **Cabal** (Haskell build tool) installed
3. **Redis** running locally or via Docker
4. A text editor with Haskell support (VS Code + Haskell extension recommended)

### First Steps

```bash
# 1. Clone the repository
git clone https://github.com/nicolasholanda/bitonic-sequence-generator.git
cd bitonic-sequence-generator

# 2. Start Redis (using Docker)
docker-compose up -d

# 3. Build the project
cabal build

# 4. Run tests
cabal test

# 5. Run the application
cabal run bitonic-api
```

---

## 📖 Understanding the Codebase

### Reading Order

**If you're new to Haskell**, we recommend reading the files in this order:

#### 1. **Data Layer** (Start Here - Easiest)
   - `app/src/BitonicModels.hs` - Simple data structures (like Java POJOs)
   - **What you'll learn**: Records, automatic JSON serialization, deriving instances

#### 2. **Core Algorithm** (Pure Logic - No Side Effects)
   - `app/src/BitonicSequence.hs` - The bitonic sequence generation algorithm
   - **What you'll learn**: Pure functions, guards, let bindings, list operations

#### 3. **Infrastructure Layer** (Dependency Injection)
   - `app/src/AppContext.hs` - Application environment and ReaderT pattern
   - **What you'll learn**: Monads, ReaderT for dependency injection, IO operations

#### 4. **Data Access Layer** (Database/Cache)
   - `app/src/BitonicRepository.hs` - Redis operations
   - **What you'll learn**: liftIO, asks, ByteString conversions, Redis operations

#### 5. **Business Logic Layer** (Validation & Orchestration)
   - `app/src/BitonicService.hs` - Business rules and caching strategy
   - **What you'll learn**: Either for error handling, where clauses, monadic composition

#### 6. **Error Handling Layer** (HTTP Error Translation)
   - `app/src/ApiError.hs` - Error types and HTTP response mapping
   - **What you'll learn**: Pattern matching, type constructors, error translation

#### 7. **HTTP Layer** (Controllers/Routes)
   - `app/src/BitonicController.hs` - HTTP endpoints and request handling
   - **What you'll learn**: Scotty web framework, exception handling, ActionM monad

#### 8. **Application Bootstrap** (Entry Point)
   - `app/src/Main.hs` - Application startup
   - **What you'll learn**: IO monad, application initialization, error handling at startup

---

## 🏗️ Architecture Overview

Our application follows a **clean architecture** pattern with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────┐
│                     HTTP Layer                          │
│  BitonicController.hs - Routes & Request Handling       │
└─────────────────┬───────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────┐
│                  Error Translation                      │
│     ApiError.hs - ServiceError → HTTP Response          │
└─────────────────┬───────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────┐
│                   Business Logic                        │
│   BitonicService.hs - Validation & Orchestration        │
└─────────────┬───────────────────────┬───────────────────┘
              │                       │
              ▼                       ▼
┌─────────────────────┐   ┌─────────────────────────────┐
│   Core Algorithm    │   │   Data Access Layer         │
│ BitonicSequence.hs  │   │  BitonicRepository.hs       │
│  (Pure Function)    │   │   (Redis Operations)        │
└─────────────────────┘   └──────────┬──────────────────┘
                                     │
                                     ▼
                          ┌─────────────────────┐
                          │   Infrastructure    │
                          │  AppContext.hs      │
                          │ (DI & Environment)  │
                          └─────────────────────┘
```

### Layer Responsibilities

| Layer | Module | Responsibility | Talks To |
|-------|--------|----------------|----------|
| **HTTP** | BitonicController | Routes, JSON parsing, HTTP responses | Service, ApiError |
| **Error Translation** | ApiError | Convert errors to HTTP status codes | Service (errors) |
| **Business Logic** | BitonicService | Validation, caching strategy | Repository, Algorithm |
| **Data Access** | BitonicRepository | Redis operations (get/set) | AppContext |
| **Algorithm** | BitonicSequence | Generate bitonic sequences | None (pure) |
| **Models** | BitonicModels | Data structures (DTOs) | All layers |
| **Infrastructure** | AppContext | Dependency injection, environment | Repository |
| **Bootstrap** | Main | Application startup | Controller, AppContext |

---

## 🔍 Common Patterns Explained

### Pattern 1: The `AppM` Monad (Dependency Injection)

**What it looks like:**
```haskell
type AppM = ReaderT AppEnv IO
```

**What it means:**
- `AppM` is our custom type for actions that need access to the application environment
- It's like having a `@Autowired` dependency in Spring or `useContext()` in React
- Functions using `AppM` can access Redis connection without passing it explicitly

**How to use it:**
```haskell
-- Get environment data
conn <- asks envRedisConn  -- Like: this.redisConnection

-- Run IO operations
result <- liftIO $ someIOAction  -- Like: await someAsyncFunction()
```

### Pattern 2: `Either` for Error Handling (No Exceptions!)

**What it looks like:**
```haskell
generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)
```

**What it means:**
- Functions return `Either Error Success` instead of throwing exceptions
- `Left` = error case (like throwing an exception)
- `Right` = success case (like returning normally)

**How to handle it:**
```haskell
result <- generateBitonic req
case result of
    Right response -> -- Success! Use response
    Left err -> -- Error! Handle err
```

### Pattern 3: The `do` Notation (Sequential Operations)

**What it looks like:**
```haskell
main :: IO ()
main = do
    conn <- connectRedis
    putStrLn "Connected"
    return ()
```

**What it means:**
- `do` is syntactic sugar for chaining monadic operations
- Like `async/await` in JavaScript
- Each line runs in sequence, can use values from previous lines

**Equivalent in JavaScript:**
```javascript
async function main() {
    const conn = await connectRedis();
    console.log("Connected");
    return;
}
```

### Pattern 4: Pattern Matching (Type-Safe Switch)

**What it looks like:**
```haskell
toHttpStatus :: ApiError -> Status
toHttpStatus (InvalidRequest _) = status400
toHttpStatus (ServiceUnavailable _) = status503
toHttpStatus (InternalError _) = status500
```

**What it means:**
- Define function multiple times, once per case
- Compiler ensures you handle ALL cases
- Much safer than `if/else` or `switch`

**Equivalent in TypeScript:**
```typescript
function toHttpStatus(err: ApiError): Status {
    switch (err.type) {
        case "InvalidRequest": return 400;
        case "ServiceUnavailable": return 503;
        case "InternalError": return 500;
    }
}
```

### Pattern 5: The `$` Operator (Parentheses Eliminator)

**What it looks like:**
```haskell
json $ toErrorResponse err
-- Same as: json (toErrorResponse err)
```

**What it means:**
- `$` means "apply function to everything on the right"
- Eliminates parentheses for cleaner code
- `f $ g $ h x` = `f(g(h(x)))`

---

## 🐛 Debugging Tips

### 1. **Understanding Type Errors**

Haskell type errors can be intimidating. Here's how to read them:

```
• Couldn't match type 'IO String' with 'String'
  Expected type: String
    Actual type: IO String
```

**Translation**: You have an `IO String` (a recipe to get a String) but need a plain `String`. Use `<-` in a `do` block or `liftIO`.

### 2. **Common Errors and Solutions**

| Error Message | Problem | Solution |
|---------------|---------|----------|
| `Couldn't match type 'IO a' with 'a'` | Forgot to extract from IO | Use `<-` or `liftIO` |
| `No instance for (Show ...)` | Can't print this type | Add `deriving (Show)` |
| `Parse error on input 'do'` | Indentation wrong | Ensure consistent indentation |
| `Couldn't match type 'AppM' with 'IO'` | Wrong monad | Use `liftIO` or `runApp` |

### 3. **Debugging Techniques**

```haskell
-- Print debug information
import Debug.Trace

-- Add trace to see values
myFunction x = trace ("x = " ++ show x) $ actualCalculation x

-- Use in do blocks
do
    traceM $ "Processing: " ++ show input
    result <- someComputation input
    return result
```

### 4. **REPL (Interactive Shell)**

```bash
# Start GHCi (Haskell REPL)
cabal repl

# Load a module
:load app/src/BitonicSequence.hs

# Test a function
bitonicArray 5 1 10

# Check type
:type bitonicArray

# Get info about a function
:info bitonicArray
```

---

## 💻 Useful Commands

### Building and Running

```bash
# Clean build
cabal clean && cabal build

# Build with warnings
cabal build --ghc-options="-Wall"

# Run application
cabal run bitonic-api

# Run specific test suite
cabal test bitonic-test
cabal test bitonic-service-test
cabal test bitonic-repository-test
cabal test bitonic-api-integration-test

# Build documentation
cabal haddock --haddock-hyperlink-source
```

### Development Workflow

```bash
# Watch mode (rebuild on file changes) - requires ghcid
ghcid --command "cabal repl"

# Format code (requires ormolu)
ormolu --mode inplace app/src/**/*.hs

# Lint code (requires hlint)
hlint app/src/
```

### Redis Operations (Debugging)

```bash
# Connect to Redis CLI
docker exec -it bitonic-sequence-generator-redis-1 redis-cli

# Check cached values
KEYS *
GET "bitonic:5:1:10"

# Clear cache
FLUSHALL
```

### Load Testing

```bash
# Install k6 (if not already)
# See: https://k6.io/docs/getting-started/installation/

# Run load tests
cd k6
k6 run cached-load-test.js
k6 run no-cache-load-test.js
k6 run mixed-cache-load-test.js
```

---

## 📚 Additional Resources

### Haskell Learning Resources

1. **[Learn You a Haskell](http://learnyouahaskell.com/)** - Beginner-friendly book
2. **[Haskell Wiki](https://wiki.haskell.org/)** - Comprehensive reference
3. **[Real World Haskell](http://book.realworldhaskell.org/)** - Practical applications
4. **[Hoogle](https://hoogle.haskell.org/)** - Search functions by name or type signature

### Project-Specific Documentation

1. **[Scotty Documentation](https://hackage.haskell.org/package/scotty)** - Web framework
2. **[Hedis Documentation](https://hackage.haskell.org/package/hedis)** - Redis client
3. **[Aeson Documentation](https://hackage.haskell.org/package/aeson)** - JSON library
4. **[ReaderT Pattern](https://www.fpcomplete.com/haskell/tutorial/monad-transformers/)** - Monad transformers

### Our Documentation

1. **[Haddock Docs](https://nicolasholanda.github.io/bitonic-sequence-generator/)** - Generated API documentation
2. **[HASKELL-CHEATSHEET.md](./HASKELL-CHEATSHEET.md)** - Quick reference for common syntax
3. **Annotated Code** - Check the `comments` branch for heavily annotated versions

---

## 🤝 Contributing

### Code Style Guidelines

1. **Use meaningful names**: `generateBitonic` not `genBit`
2. **Keep functions pure when possible**: No IO unless necessary
3. **Add type signatures**: Always declare function types
4. **Document with Haddock**: Add `{-| ... -}` comments for public functions
5. **Handle errors with Either**: Avoid exceptions in business logic

### Adding a New Endpoint

Example: Adding a `GET /health` endpoint

```haskell
-- 1. Add to BitonicController.hs
routes :: AppEnv -> ScottyM ()
routes env = do
    post "/bitonic" $ do
        -- existing code...
    
    -- New endpoint
    get "/health" $ do
        json $ object ["status" .= ("healthy" :: String)]
```

### Adding a New Service Function

Example: Adding validation logic

```haskell
-- 1. Add to BitonicService.hs
validateRange :: BitonicRequest -> Either ServiceError ()
validateRange req
    | r req - l req < n req = Left $ InvalidParameters "Range too small"
    | otherwise = Right ()

-- 2. Use in existing functions
generateBitonic :: BitonicRequest -> AppM (Either ServiceError BitonicResponse)
generateBitonic req = do
    case validateRequest req of
        Left err -> return $ Left err
        Right _ -> case validateRange req of  -- Add new validation
            Left err -> return $ Left err
            Right _ -> -- continue...
```

---

## 🎯 Quick Start Checklist

- [ ] Redis is running (`docker-compose up -d`)
- [ ] Project builds successfully (`cabal build`)
- [ ] Tests pass (`cabal test`)
- [ ] Read `BitonicModels.hs` (easiest file)
- [ ] Read `BitonicSequence.hs` (pure algorithm)
- [ ] Explored the annotated code in `comments` branch
- [ ] Reviewed `HASKELL-CHEATSHEET.md`
- [ ] Successfully made a POST request to `/bitonic`
- [ ] Checked Redis for cached values

---

## 💡 Tips for Success

1. **Don't panic about monads** - Think of them as containers with special rules
2. **Type errors are your friend** - They catch bugs before runtime
3. **Use the REPL** - Test small pieces of code interactively
4. **Read type signatures first** - They tell you what a function does
5. **Pattern matching is powerful** - Embrace it instead of if/else
6. **Start small** - Understand one module at a time

---

## 🆘 Getting Help

If you're stuck:

1. Check the annotated code in the `comments` branch
2. Look at the Haddock documentation
3. Review the HASKELL-CHEATSHEET.md
4. Search [Hoogle](https://hoogle.haskell.org/) for function signatures
5. Ask the team - we're all learning together!

---

**Happy Coding! 🚀**

Remember: Haskell has a steep learning curve, but the payoff in code quality and correctness is worth it!
