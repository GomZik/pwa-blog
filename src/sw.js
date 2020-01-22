importScripts('https://storage.googleapis.com/workbox-cdn/releases/4.3.1/workbox-sw.js')

self.workbox.routing.registerRoute(
  /\.*/,
  new self.workbox.strategies.NetworkFirst()
)
