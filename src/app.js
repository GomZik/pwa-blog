import { Elm } from './Main.elm'

window.addEventListener('load', () => {
  navigator.serviceWorker.register('./sw.js')
})

Elm.Main.init()
