import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate, Counter } from 'k6/metrics';

const errorRate = new Rate('errors');
const cacheHits = new Counter('cache_hits');
const cacheMisses = new Counter('cache_misses');

export const options = {
  stages: [
    { duration: '30s', target: 20 },
    { duration: '1m', target: 20 },
    { duration: '30s', target: 0 },
  ],
  thresholds: {
    http_req_duration: ['p(95)<300'],
    errors: ['rate<0.05'],
  },
};

const BASE_URL = 'http://localhost:3000';

const cachedParams = { n: 500, l: 50, r: 300 };

export default function () {
  const useCache = Math.random() < 0.7;
  
  let payload;
  if (useCache) {
    payload = JSON.stringify(cachedParams);
    cacheHits.add(1);
  } else {
    const l = Math.floor(Math.random() * 100) + 1;
    const r = l + Math.floor(Math.random() * 400) + 100;
    const maxN = (r - l) * 2 + 1;
    const n = Math.floor(Math.random() * (maxN - 10)) + 10;
    
    payload = JSON.stringify({ n, l, r });
    cacheMisses.add(1);
  }

  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
  };

  const res = http.post(`${BASE_URL}/bitonic`, payload, params);

  const success = check(res, {
    'status is 200': (r) => r.status === 200,
    'cached requests faster': (r) => !useCache || r.timings.duration < 50,
  });

  errorRate.add(!success);

  sleep(0.5);
}
