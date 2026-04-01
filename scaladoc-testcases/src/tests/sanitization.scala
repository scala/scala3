package tests.sanitization

/** <script>alert('hello')</script> */
class Script

/** < script   >alert('hello')</script
> */
class ScriptWithSpaces

/** <script>alert('hello')</script> */
class FakeSafeScript