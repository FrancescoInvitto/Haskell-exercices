type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

myPhoneBook = [("A", "123"), ("C", "456"), ("E", "789")]

-- with recursion
getPhoneNumber :: Name -> PhoneBook -> PhoneNumber
getPhoneNumber "" _ = "No name specified"
getPhoneNumber name [] = "No phone number for " ++ name
getPhoneNumber name ((n, p) : xs)
  | (name == n) = "Phone number for " ++ name ++ " is " ++ p
  | otherwise = getPhoneNumber name xs

-- with tail recursion
getPhoneNumber' :: Name -> PhoneBook -> PhoneBook -> PhoneNumber
getPhoneNumber' "" _ _= "No name specified"
getPhoneNumber' name [] acc = "No phone number for " ++ name
getPhoneNumber' name ((n, p) : xs) acc
  | (name == n) = "Phone number for " ++ name ++ " is " ++ p
  | otherwise = getPhoneNumber' name xs ((n, p) : acc)

-- with fold
getPhoneNumber'' :: Name -> PhoneBook -> PhoneNumber
getPhoneNumber'' "" _  = "No name specified"
getPhoneNumber'' name phonebook = foldr (\(n, p) acc -> if n == name then found ++ p else acc) notFound phonebook
  where found = "Phone number for " ++ name ++ " is "
        notFound = "No phone number for " ++ name

--with maybe
getPhoneNumber''' :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber''' "" _ = Nothing
getPhoneNumber''' name phonebook = foldr (\(n, p) acc -> if n == name then Just p else acc) Nothing phonebook

