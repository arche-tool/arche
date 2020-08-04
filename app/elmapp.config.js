const { createProxyMiddleware } = require('http-proxy-middleware');

module.exports = {
  setupProxy: function(app) {
    app.use(createProxyMiddleware('/compute-api', {
        target: 'http://localhost:8080/',
        pathRewrite: {
          '/async$' : ''
        },
        logLevel: "debug"
    }));

    app.use(createProxyMiddleware('/api', {
        target: 'http://localhost:8080/',
        pathRewrite: {
          '/async$' : ''
        },
        logLevel: "debug"
    }));
  }
};