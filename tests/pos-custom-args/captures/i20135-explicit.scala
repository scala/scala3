import language.experimental.captureChecking

class Network

class Page(val nw: Network^):
  def render(client: Page^{nw} -> Unit) = client(this)

def main(net: Network^) =
  var page: Page{val nw: Network^{net}}^{net} = Page(net)
  page.render(p => ())

