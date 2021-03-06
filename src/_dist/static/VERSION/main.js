addEventListener("DOMContentLoaded", main)

function main() {
  const app = Elm.Main.init({
    node: element(document.body, "div"),
    flags: {},
  })
}

function element(parent, tag, className) {
  const el = document.createElement(tag)
  if (parent) parent.appendChild(el)
  if (className) el.className = className
  return el
}
