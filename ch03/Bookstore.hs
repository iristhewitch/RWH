-- file: ch03/Bookstore.hs

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

-- this:
data Customer = Customer {
        -- record syntax
        customerID      :: CustomerID,
        customerName    :: String,
        customerAddress :: Address
} deriving (Show)

-- completely replaces this:
-- data Customer = Customer Int String [String]
--                 deriving (Show)

-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

customer1 = Customer 271828 "J.R. Hacker" ["255 Syntax Ct.", "Milpitas, CA 95134", "USA"]
customer2 = Customer {
                -- record syntax; fields can be out of order
                customerID = 271829,
                customerAddress = ["1048576 Disk Drive", "Milpitas, CA 95134", "USA"],
                customerName = "Jane Q. Citizen"
            }