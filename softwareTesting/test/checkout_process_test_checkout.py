from unittest.mock import patch
import pytest
from online_shopping_cart.user.user import User
from online_shopping_cart.checkout.shopping_cart import ShoppingCart

# Mock `get_products` globally before importing `checkout_process`
patcher = patch('online_shopping_cart.product.product_data.get_products')
patcher.start()

# Import after mock
from online_shopping_cart.checkout.checkout_process import checkout

patcher.stop()


# Mock Product Class for testing
class MockProduct:
    def __init__(self, name, price, units):
        self.name = name
        self.price = price
        self.units = units


######################################################################
###############10 test cases where the input type is valid###########
######################################################################

# Case1 Test User with Sufficient Wallet and Multiple Items with Decimal Prices
@patch('builtins.print')
def test_decimal_prices(mock_print):
    """Test purchasing items with decimal prices. Testing Cumulative Precision in Decimal Arithmetic. testing adding opention"""
    user = User(name="User_A", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Grapes", price=2.99, units=3))
    cart.add_item(MockProduct(name="Orange", price=1.49, units=4))
    checkout(user, cart)
    mock_print.assert_any_call("Thank you for your purchase, User_A! Your remaining balance is 85.07")
    assert user.wallet == 85.07
    assert cart.is_empty()

# Case2 Test User with Wallet Balance Equal to Total Price After Rounding
@patch('builtins.print')
def test_wallet_equal_to_total_after_rounding(mock_print):
    """Test user wallet equal to total price after rounding issues. Testing Equality Determination for Floating Point Comparison. testing equuality operation"""
    user = User(name="User_B", wallet=29.97)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Notebook", price=9.99, units=3))
    checkout(user, cart)
    mock_print.assert_any_call("Thank you for your purchase, User_B! Your remaining balance is 0.0")
    assert user.wallet == 0.0
    assert cart.is_empty()


#Case 3 Test Checkout with Maximum Integer Quantity
@patch('builtins.print')
def test_max_integer_quantity(mock_print):
    """Test purchasing with maximum integer quantity."""
    user = User(name="User_C", wallet=1e9)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Pencil", price=0.50, units=2**31 - 1))
    checkout(user, cart)
    # Expecting failure message because total price exceeds wallet balance
    mock_print.assert_any_call("You don't have enough money to complete the purchase. Please try again!")
    assert user.wallet == 1e9  # Wallet remains unchanged
    assert not cart.is_empty()  # Cart remains unchanged



#Case4 Test Cart with One Item Having Zero Quantity
@patch('builtins.print')
def test_item_with_zero_quantity(mock_print):
    """Test cart containing an item with zero quantity."""
    user = User(name="User_D", wallet=50.0)
    cart = ShoppingCart()
    item = MockProduct(name="Book", price=25.0, units=0)

    # Validate quantity before adding to the cart
    if item.units > 0:
        cart.add_item(item)

    checkout(user, cart)

    # Since the item with zero quantity wasn't added, the cart is considered empty
    mock_print.assert_any_call('Your basket is empty. Please add items before checking out.')
    assert user.wallet == 50.0
    assert cart.is_empty()


#case 5 The Number of Purchase is exactly the Same as the Number in Stock
@patch('builtins.print')
def test_items_matching_stock(mock_print):
    """Test purchasing items equal to stock."""
    user = User(name="User_E", wallet=20.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Banana", price=1.0, units=12))
    checkout(user, cart)
    mock_print.assert_any_call("Thank you for your purchase, User_E! Your remaining balance is 8.0")
    assert user.wallet == 8.0
    assert cart.is_empty()

# case6 Test User with Wallet as int and float Type

@patch('builtins.print')
def test_user_with_valid_wallet(mock_print, valid_cart):
    """Test checkout with user having a valid wallet."""
    user = User(name="ValidUser", wallet=100.0)  # float type for wallet
    cart = valid_cart  # Assume valid_cart is a fixture providing a cart with items
    cart.get_total_price = lambda: 50.0  # Mock total price
    checkout(user, cart)
    assert user.wallet == 50.0  # Wallet should decrease by the total price
    mock_print.assert_called_with("Thank you for your purchase, ValidUser! Your remaining balance is 50.0")



# Case 7 Test Checkout with Mixed Valid and Invalid Items
@patch('builtins.print')
def test_mixed_valid_invalid_items(mock_print):
    """Test cart with valid and invalid items."""
    user = User(name="User_G", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Chair", price=30.0, units=1))
    cart.add_item(MockProduct(name="InvalidItem", price=-20.0, units=1))  # Negative price

    # Validate and remove invalid items
    cart.items = [item for item in cart.items if item.price > 0]

    # Proceed with checkout
    checkout(user, cart)

    # Verify the final output
    mock_print.assert_any_call("Thank you for your purchase, User_G! Your remaining balance is 70.0")
    assert user.wallet == 70.0
    assert cart.is_empty()


#Case 8 Test User with Sufficient Wallet but Empty Cart After Removing Items
@patch('builtins.print')
def test_empty_cart_after_removing_items(mock_print):
    """Test checkout after cart items are removed."""
    user = User(name="User_H", wallet=100.0)
    cart = ShoppingCart()
    item = MockProduct(name="Table", price=80.0, units=1)
    cart.add_item(item)
    cart.remove_item(item)
    checkout(user, cart)
    mock_print.assert_any_call('Your basket is empty. Please add items before checking out.')
    assert user.wallet == 100.0
    assert cart.is_empty()


#Case 9 Test User with Exact Change Including Cents
@patch('builtins.print')
def test_exact_change_with_cents(mock_print):
    """Test user has exact change including cents."""
    user = User(name="User_I", wallet=5.75)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Coffee", price=2.50, units=2))
    cart.add_item(MockProduct(name="Donut", price=0.75, units=1))
    checkout(user, cart)
    mock_print.assert_any_call("Thank you for your purchase, User_I! Your remaining balance is 0.0")
    assert user.wallet == 0.0
    assert cart.is_empty()



#Case 10
@patch('builtins.print')
def test_purchasing_all_stock(mock_print):
    """Test user purchasing all available stock of different items."""
    user = User(name="User_J", wallet=500.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="Apple", price=2.0, units=10))  # All Apple stock
    cart.add_item(MockProduct(name="Pineapple", price=1.0, units=15))  # All Banana stock
    checkout(user, cart)
    mock_print.assert_any_call("Thank you for your purchase, User_J! Your remaining balance is 465.0")
    assert user.wallet == 465.0
    assert cart.is_empty()


#####################################

# Fixtures to create new user with
@pytest.fixture
def valid_user():
    """Fixture for a valid user with sufficient wallet balance."""
    return User(name="User", wallet=100.0)


@pytest.fixture
def valid_cart():
    """Fixture for a valid shopping cart with items."""
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="item1", price=25.0, units=1))
    cart.add_item(MockProduct(name="item2", price=25.0, units=1))
    return cart

####################################


#####################################################
################Test invalid user input##############
#####################################################


'''
Invalid Input Type Test Cases (Covering int, float, string, list)
'''

#Case 1: Test User as Integer
def test_user_as_integer(valid_cart):
    """Test checkout with user as integer."""
    with pytest.raises(AttributeError):
        checkout(12345, valid_cart)

#Case 2: Test User as Float
def test_user_as_float(valid_cart):
    """Test checkout with user as float."""
    with pytest.raises(AttributeError):
        checkout(123.45, valid_cart)

#Case 3: Test User as String
def test_user_as_string(valid_cart):
    """Test checkout with user as string."""
    with pytest.raises(AttributeError):
        checkout("NotAUserObject", valid_cart)

#Case4: Test User as List
def test_user_as_list(valid_cart):
    """Test checkout with user as list."""
    with pytest.raises(AttributeError):
        checkout(["User", 100], valid_cart)

#Case5: Test Cart as Integer
def test_cart_as_integer(valid_user):
    """Test checkout with cart as integer."""
    with pytest.raises(AttributeError):
        checkout(valid_user, 12345)

#Case6: Test Cart as Float
def test_cart_as_float(valid_user):
    """Test checkout with cart as float."""
    with pytest.raises(AttributeError):
        checkout(valid_user, 123.45)

#Case7: Test Cart as String
def test_cart_as_string(valid_user):
    """Test checkout with cart as string."""
    with pytest.raises(AttributeError):
        checkout(valid_user, "NotACartObject")

#case8: Test Cart as List
def test_cart_as_list(valid_user):
    """Test checkout with cart as list."""
    with pytest.raises(AttributeError):
        checkout(valid_user, ["Item1", "Item2"])



'''
Additional Cases
'''

#Case9: test cart with none item
@patch('builtins.print')
def test_cart_with_none_items(mock_print, valid_user):
    """Test checkout with cart having None as items."""
    cart = ShoppingCart()
    cart.items = None
    checkout(valid_user, cart)
    mock_print.assert_any_call("Your basket is empty. Please add items before checking out.")


#Case10 :Test Item with Non-Numeric Units/Price
@patch('builtins.print')
def test_item_with_non_numeric_units(mock_print):
    """Test purchasing an item with non-numeric units."""
    user = User(name="Taylor Swift", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="reputation", price=10.0, units="Ten"))
    with pytest.raises(TypeError):
        checkout(user, cart)

@patch('builtins.print')
def test_item_with_non_numeric_price(mock_print):
    """Test purchasing an item with non-numeric price."""
    user = User(name="Chris Hemsworth", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="hammer", price="Free", units=1))
    with pytest.raises(TypeError):
        checkout(user, cart)



#Case11 :Test Negative Quantity/Price in Cart
@patch('builtins.print')
def test_negative_quantity_in_cart(mock_print):
    """Test purchasing an item with negative quantity."""
    user = User(name="Micheal Jordan", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="basketball", price=20.0, units=-5))
    cart.items = [item for item in cart.items if item.units > 0]

    checkout(user, cart)

    mock_print.assert_any_call("Your basket is empty. Please add items before checking out.")
    assert user.wallet == 100.0
    assert cart.is_empty()


@patch('builtins.print')
def test_negative_price_in_cart(mock_print):
    """Test purchasing an item with negative price."""
    user = User(name="Robert Lewandowski", wallet=100.0)
    cart = ShoppingCart()
    cart.add_item(MockProduct(name="GoldBall", price=-50.0, units=1))

    #filter our negative price
    cart.items = [item for item in cart.items if item.price > 0]
    checkout(user, cart)
    mock_print.assert_any_call("Your basket is empty. Please add items before checking out.")
    assert user.wallet == 100.0
    assert cart.is_empty()

#Case 12: Null user or Null cart
def test_null_user(valid_cart):
    """Test checkout with a null user."""
    with pytest.raises(AttributeError):
        checkout(None, valid_cart)


def test_null_cart(valid_user):
    """Test checkout with a null cart."""
    with pytest.raises(AttributeError):
        checkout(valid_user, None)