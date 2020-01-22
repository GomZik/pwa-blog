// importScripts('https://storage.googleapis.com/workbox-cdn/releases/4.3.1/workbox-sw.js')
import 'workbox-sw/build/workbox-sw'

console.log(self.workbox)

self.workbox.precaching.precacheAndRoute([
  'index.html'
])

self.workbox.routing.registerRoute(
  /\.*/,
  new self.workbox.strategies.NetworkFirst()
)
