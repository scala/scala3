import language.experimental.captureChecking

class Network

class Page(using nw: Network^):
  def render(client: Page^{nw} ?-> Unit) = client(using this)

def main(net: Network^) =
  var page = Page(using net)
  page.render(())

