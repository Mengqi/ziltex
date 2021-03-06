//
// @author mengqi
//

'use strict';

const http = require('http');
const redis = require('redis');
const express = require('express');

console.log(process.env.REDIS_PORT_6379_TCP_ADDR + ':' + process.env.REDIS_PORT_6379_TCP_PORT);

const app = express();
const client = redis.createClient('6379', 'redis');

app.use(express.static('public'));

app.get('/count', function(req, res, next) {
  client.incr('counter', function(err, counter) {
    if(err) return next(err);
    res.send('This page has been viewed ' + counter + ' times!');
  });
});

http.createServer(app).listen(process.env.PORT || 8080, function() {
  console.log('Listening on port ' + (process.env.PORT || 8080));
});

