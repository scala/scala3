

import language.`future-migration`

def test1 = { implicit (x: Int) => x + x } // warn

def test2 = { implicit x: Int => x + x } // warn // warn
