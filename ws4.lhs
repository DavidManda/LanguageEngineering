1.1

> import Text.Yoda

1.1.1
1.
   a) parse (word <~> number) "hello234 parser" = [(("hello", 234)," parser")]
   b) parse (word <~> number) "hello 123" = [(("hello", 123),"")]
   c) parse (word *> number) "Dog14a" =  [(14,"a")]\
2.
   a) parse ((++) <$> word <*> pure " world") "hello parser" = [("hello world"),"parser"]
