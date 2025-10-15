# K6 Performance Tests

## Test Suites

### Load Test
Tests normal load conditions.

**Run:**
```bash
k6 run k6/load-test.js
```

**Scenario:**
- Ramp up to 10 users over 30s
- Maintain 10 users for 1 minute
- Ramp down to 0 over 30s

**Thresholds:**
- p95 response time < 500ms
- p99 response time < 1000ms
- Error rate < 10%

### Stress Test
Pushes the API to its limits with high load.

**Run:**
```bash
k6 run k6/stress-test.js
```

**Scenario:**
- Ramp up to 50 users over 1 minute
- Maintain 50 users for 2 minutes
- Ramp up to 100 users over 1 minute
- Maintain 100 users for 2 minutes
- Ramp down to 0

**Thresholds:**
- p95 response time < 1000ms
- p99 response time < 2000ms
- Error rate < 20%

### Cache Test
Tests Redis cache hit vs miss performance.

**Run:**
```bash
k6 run k6/cache-test.js
```

**Scenario:**
- 70% requests use same parameters (cache hits)
- 30% requests use random parameters (cache misses)
- 20 concurrent users

**Thresholds:**
- p95 response time < 300ms
- Error rate < 5%

## Generating HTML Reports

Install k6 HTML reporter:
```bash
npm install -g k6-html-reporter
```

Run with JSON output:
```bash
k6 run --out json=results.json k6/load-test.js
k6-html-reporter results.json
```

## Running All Tests

```bash
k6 run k6/load-test.js
k6 run k6/stress-test.js
k6 run k6/cache-test.js
```
