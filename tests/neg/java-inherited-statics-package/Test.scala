package b

// package access statics are only inherited within their package (JLS 8.4.8)
def test =
  SameFacade.packageAccess() // ok
  a.OtherFacade.packageAccess() // error
