// Note: In ServiceWorker, `self` points to `ServiceWorkerGlobalScope`.
// <https://developer.mozilla.org/ja/docs/Web/API/ServiceWorkerGlobalScope>

log('ServiceWorker script loaded!')

const STORAGE = 'zephyr_v17' // increment version to enforce old cache deletion
const APPSHELL = [
  '/',
  '/index.html',
  '/zephyr.js',
  '/images/icons/icon-192x192.png',
  '/images/icons/icon-512x512.png',
]

self.addEventListener('install', (e) => {
  log(`Installing... (AppShell version: ${STORAGE})`)
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
        log(`Deleting old cache storage: ${cacheName}`)
        return caches.delete(cacheName)
      }
    }))
  })
}

self.addEventListener('fetch', (e) => {
  if (e.request.url.startsWith("http")) {
    if (isAppShellFile(e.request.url)) {
      log(`AppShell fetch: ${e.request.url}`)
      return respondAppShell(e)
    } else {
      return respondOther(e)
    }
  } else {
    // Let browser work in default way for non-http fetch; this includes chrome-extension:// and such
    return
  }
})

function isAppShellFile(url) {
  return url === self.origin || APPSHELL.some((path) => url === self.origin + path)
}

function respondAppShell(e) {
  e.respondWith(caches.open(STORAGE).then((cache) => {
    return cache.match(e.request).then((cachedResponse) => {
      // Update cache in background
      fetch(e.request).then((newResponse) => {
        // Cache update check depends on ETag;
        // thankfully, elm-live dev server and GitHub Pages both calculate and return ETags
        if (newResponse.headers.get('etag') !== cachedResponse.headers.get('etag')) {
          cache.put(e.request, newResponse.clone())
          log(`Cache updated (${e.request.url}). Reload to activate.`)
        }
      }).catch(() => {
        // Do nothing if cache update request failed; current cache lives on.
      })
      log(`Using cached AppShell (${e.request.url}).`)
      return cachedResponse
    }).catch(() => {
      // There is no cached AppShell; basically should not happen.
      return fetch(e.request).then((newResponse) => {
        cache.put(e.request, newResponse.clone())
        log(`Cache loaded (${e.request.url}).`)
        return newResponse
      })
    })
  }))
}

function respondOther(e) {
  // Network only mode. Not caching at all. Respect `e.request.mode`.
  e.respondWith(fetch(e.request))
}

function log(msg) {
  console.log(`[ZephyrSW] ${msg}`)
}
