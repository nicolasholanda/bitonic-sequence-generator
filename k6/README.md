# K6 Performance Tests

## Test Suites

### Cached Load Test
Tests performance with cache hits using static parameters.

**Run:**
```bash
k6 run k6/cached-load-test.js
```

**Scenario:**
- All requests use same parameters (n=500, l=50, r=300)
- Tests Redis cache hit performance
- Constant VUs

**Purpose:**
- Measure best-case performance with full cache utilization

### No Cache Load Test
Tests performance with cache misses using random parameters.

**Run:**
```bash
k6 run k6/no-cache-load-test.js
```

**Scenario:**
- All requests use random parameters with large ranges
- l: random between 1-100
- r: random between (l+100) to 500
- Tests worst-case performance (no cache hits)

**Purpose:**
- Measure performance under heavy computation load
- Find maximum capacity with no caching benefits

### Mixed Cache Load Test
Tests realistic workload with mixed cache hits and misses.

**Run:**
```bash
k6 run k6/mixed-cache-load-test.js
```

**Scenario:**
- 70% requests use static parameters (cache hits)
- 30% requests use random parameters (cache misses)
- Simulates real-world usage patterns

**Purpose:**
- Measure performance under realistic conditions
- Determine how many concurrent users the application supports

## Generating HTML Reports

Install k6 HTML reporter:
```bash
npm install -g k6-html-reporter
```

Run with JSON output:
```bash
k6 run --out json=results.json k6/cached-load-test.js
k6-html-reporter results.json
```

## Running All Tests

```bash
k6 run k6/cached-load-test.js
k6 run k6/no-cache-load-test.js
k6 run k6/mixed-cache-load-test.js
```
