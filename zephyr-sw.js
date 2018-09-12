log('ServiceWorker script loaded!')

const STORAGE = 'zephyr_cache_v6' // increment version to enforce old cache deletion
const APPSHELL = [
  '/',
  "/index.html",
  "/zephyr.js",
  "/vendor/WebSocketClient.js",
  "/images/icons/icon-192x192.png",
  "/images/icons/icon-512x512.png",
]

self.addEventListener('install', (e) => {
  log('Installing...')
  e.waitUntil(
    caches.open(STORAGE).then((cache) => {
      log(`Adding AppShell to Cache: ${APPSHELL}`)
      return cache.addAll(APPSHELL)
    }).then(() => {
      log('Done.')
      return self.skipWaiting()
    })
  )
})

self.addEventListener('activate', (e) => {
  log('Activating...')
  e.waitUntil(
    deleteOldCaches().then(() => {
      log('Done.')
      return self.clients.claim()
    })
  )
})

function deleteOldCaches() {
  return caches.keys().then((cacheNames) => {
    return Promise.all(cacheNames.map((cacheName) => {
      if (cacheName !== STORAGE) {
        return caches.delete(cacheName)
      }
    }))
  })
}

self.addEventListener('fetch', (e) => {
  log(`Fetch event: ${e.request.url}`)
  if (e.request.url.startsWith("http")) {
    e.respondWith(
      caches.match(e.request).then((response) => {
        if (response != null) {
          log(`Using cache for: ${e.request.url}`)
          return response
        } else {
          return fetch(e.request.url, { mode: 'no-cors' })
        }
      })
    )
  } else {
    return // Let browser work in default way
  }
})

function log(msg) {
  console.log(`[ZephyrSW] ${msg}`)
}
