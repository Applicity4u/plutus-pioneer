{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}

contract :: Contract
contract = 
    When
        [Case
            (Deposit
                (Role "charlie")
                (Role "charlie")
                (Token "" "")
                (Constant 20)
            )
            (When
                [Case
                    (Deposit
                        (Role "alice")
                        (Role "alice")
                        (Token "" "")
                        (Constant 10)
                    )
                    (When
                        [Case
                            (Deposit
                                (Role "bob")
                                (Role "bob")
                                (Token "" "")
                                (Constant 10)
                            )
                            (When
                                [Case
                                    (Choice
                                        (ChoiceId
                                            "Winner"
                                            (Role "charlie")
                                        )
                                        [Bound 1 2]
                                    )
                                    (If
                                        (ValueEQ
                                            (ChoiceValue
                                                (ChoiceId
                                                    "Winner"
                                                    (Role "charlie")
                                                ))
                                            (Constant 1)
                                        )
                                        (Pay
                                            (Role "bob")
                                            (Account (Role "alice"))
                                            (Token "" "")
                                            (Constant 10)
                                            Close 
                                        )
                                        (Pay
                                            (Role "alice")
                                            (Account (Role "bob"))
                                            (Token "" "")
                                            (Constant 10)
                                            Close 
                                        )
                                    )]
                                30
                                (Pay
                                    (Role "charlie")
                                    (Account (Role "alice"))
                                    (Token "" "")
                                    (Constant 10)
                                    (Pay
                                        (Role "charlie")
                                        (Account (Role "bob"))
                                        (Token "" "")
                                        (Constant 10)
                                        Close 
                                    )
                                )
                            )]
                        20 Close 
                    )]
                10 Close 
            )]
        40 Close 
