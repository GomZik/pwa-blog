export default {
  setup: app => {
    console.log('[storage]: set up storage')
    if ( !window.localStorage ) return

    if ( app.ports.setItem ) {
      app.ports.setItem.subscribe(data => {
        window.localStorage.setItem(data.item, JSON.stringify(data.data))
      })
    }

    if ( app.ports.loadDataRequest ) {
      app.ports.loadDataRequest.subscribe(data => {
        let resp = window.localStorage.getItem(data.keyName)

        if ( app.ports.loadDataResponse ) {
          app.ports.loadDataResponse.send({
            requestId: data.requestId,
            data: resp
          })
        }
      })
    }
  }
}
