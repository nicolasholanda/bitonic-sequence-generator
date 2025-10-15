import http from 'k6/http';
import { check, sleep } from 'k6';
import { Rate } from 'k6/metrics';

const errorRate = new Rate('errors');

export const options = {
  stages: [
    { duration: '1m', target: 50 },
    { duration: '2m', target: 100 },
    { duration: '1m', target: 0 },
  ],
  thresholds: {
    http_req_duration: ['p(95)<500', 'p(99)<1000'],
    errors: ['rate<0.1'],
  },
};

const BASE_URL = 'http://localhost:3000';

export default function () {
  const payload = JSON.stringify({
    n: 500,
    l: 50,
    r: 300,
  });

  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
  };

  const res = http.post(`${BASE_URL}/bitonic`, payload, params);

  const success = check(res, {
    'status is 200': (r) => r.status === 200,
    'response has result': (r) => JSON.parse(r.body).result !== undefined,
    'result is array': (r) => Array.isArray(JSON.parse(r.body).result),
  });

  errorRate.add(!success);

  sleep(1);
}
