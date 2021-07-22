import { Elm } from './Main.elm'

const STORAGE_NS = "todos"

const app = Elm.Main.init({
  node: document.getElementById("elm-root"),
  flags: {
    todos: localStorage.getItem(STORAGE_NS),
  }
})
