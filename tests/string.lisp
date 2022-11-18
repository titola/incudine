(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(deftest square-bracket-string.1
#[[Reader syntax to begin and end
a \/\/\/ """string""" \/\/\/ as an
alternative to double-quote `"'.]]

"Reader syntax to begin and end
a \\/\\/\\/ \"\"\"string\"\"\" \\/\\/\\/ as an
alternative to double-quote `\"'.")
