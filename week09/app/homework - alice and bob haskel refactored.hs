{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}


-- Use overloaded string instead of Role constructor
alice, bob, charlie :: Party
alice   = "Alice"
bob     = "Bob"
charlie = "Charlie"

deposit :: Value
deposit = Constant 10

charlieDeposit :: Value
charlieDeposit = Constant 20

choiceId :: ChoiceId
choiceId = ChoiceId "Winner" charlie

contract :: Contract
contract = 
    When
        [Case
            (Deposit
                charlie
                charlie
                ada
                charlieDeposit
            )
            (When
                [Case
                    (Deposit
                        alice
                        alice
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Deposit
                                bob
                                bob
                                ada
                                deposit
                            )
                            (When
                                [Case
                                    (Choice
                                        choiceId
                                        [Bound 1 2]
                                    )
                                    (If
                                        (ValueEQ
                                            (ChoiceValue choiceId)
                                            (Constant 1)
                                        )
                                        (Pay
                                            bob
                                            (Account alice)
                                            ada
                                            deposit
                                            Close 
                                        )
                                        (Pay
                                            alice
                                            (Account bob)
                                            ada
                                            deposit
                                            Close 
                                        )
                                    )]
                                30
                                (Pay
                                    charlie
                                    (Account alice)
                                    ada
                                    deposit
                                    (Pay
                                        charlie
                                        (Account bob)
                                        ada
                                        deposit
                                        Close 
                                    )
                                )
                            )]
                        20 Close 
                    )]
                10 Close 
            )]
        40 Close 
