import { Elm } from './Main.elm'
import init from './Effect/Program'

window.addEventListener('load', () => {
  navigator.serviceWorker.register('./sw.js')
})

init(Elm.Main.init, {})
