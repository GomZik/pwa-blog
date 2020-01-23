import Storage from './Storage'


export default (elm, flags) => {
  const app = elm({
    flags
  })
  Storage.setup(app)
  return app
}
