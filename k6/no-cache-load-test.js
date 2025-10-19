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
    http_req_duration: ['p(95)<500', 'p(99)<800'],
    errors: ['rate<0.2'],
  },
};

const BASE_URL = 'http://localhost:3000';

export default function () {
  const l = Math.floor(Math.random() * 100) + 1;
  const r = l + Math.floor(Math.random() * 400) + 100;
  const maxN = (r - l) * 2 + 1;
  const n = Math.floor(Math.random() * (maxN - 10)) + 10;
  
  const payload = JSON.stringify({ n, l, r });

  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
  };

  const res = http.post(`${BASE_URL}/bitonic`, payload, params);

  const success = check(res, {
    'status is 200': (r) => r.status === 200,
    'response time < 500ms': (r) => r.timings.duration < 500,
  });

  errorRate.add(!success);

  sleep(0.5);
}
