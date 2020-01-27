import 'workbox-sw/build/workbox-sw'

console.log(self.workbox)

self.workbox.precaching.precacheAndRoute([
  { url: '/', revision: '5' }
])

self.workbox.routing.registerRoute(
  /\.*/,
  new self.workbox.strategies.NetworkFirst()
)
