import pytest
from online_shopping_cart.checkout.checkout_process import check_cart
from online_shopping_cart.product.product_data import Product
from online_shopping_cart.user.user import User
from online_shopping_cart.checkout.shopping_cart import ShoppingCart

# Mocking UserInterface.get_user_input
@pytest.fixture
def mock_user_input(monkeypatch):
    inputs = iter([])

    def mock_input(prompt):
        try:
            return next(inputs)
        except StopIteration:
            return "n"  # Default response

    def set_input(new_inputs):
        nonlocal inputs
        inputs = iter(new_inputs)

    monkeypatch.setattr('online_shopping_cart.user.user_interface.UserInterface.get_user_input', mock_input)
    return set_input

@pytest.fixture
def mock_user():
    return User(name="test_user", wallet=100.0)

@pytest.fixture
def mock_cart():
    cart = ShoppingCart()
    cart.add_item(Product(name="Product1", price=10.0, units=1))
    cart.add_item(Product(name="Product2", price=20.0, units=2))
    return cart

@pytest.fixture
def mock_global_products(monkeypatch):
    mock_products = [
        Product(name="Product1", price=10.0, units=10),
        Product(name="Product2", price=20.0, units=10)
    ]
    # Mock the global_products variable in the module where check_cart is defined
    monkeypatch.setattr('online_shopping_cart.checkout.checkout_process.global_products', mock_products)
    return mock_products

@pytest.fixture
def mock_display_cart_items(mocker):
    mock_display = mocker.patch('online_shopping_cart.checkout.checkout_process.display_cart_items')
    return mock_display



######################################################
############### 10 Valid Input Test Cases ############
######################################################

@pytest.mark.parametrize(
    "inputs, cart_items, wallet_balance, expected_checkout_called, expected_result",
    [
        # 1. User chooses to checkout, and their wallet matches the total price of the cart.
        (['y'], [Product(name="P1", price=50, units=1)], 50, True, True),

        # 2. User chooses to checkout, but their wallet balance is insufficient.
        (['y'], [Product(name="P1", price=100, units=2)], 150, True, False),

        # 3. User chooses no action, and the cart remains unchanged.
        (['n'], [Product(name="P1", price=30, units=1), Product(name="P2", price=20, units=1)], 100, False, False),

        # 4. User chooses to checkout an empty cart.
        (['y'], [], 100, False, False),

        # 5. User removes the only item in the cart, then tries to checkout.
        (['n', 'y', '1'], [Product(name="P1", price=20, units=1)], 100, False, False),

        # 6. User removes one item in the cart and successfully checks out with the remaining items.
        (['n', 'y', '2'], [Product(name="P1", price=20, units=1), Product(name="P2", price=30, units=1)], 50, False,
         False),
        (['y'], [Product(name="P1", price=20, units=1)], 50, True, True),

        # 7. User repeatedly chooses not to act, and no checkout occurs.
        (['n', 'n'], [Product(name="P1", price=10, units=1)], 100, False, False),

        # 8. User chooses to checkout multiple times; only one checkout occurs.
        (['y', 'y'], [Product(name="P1", price=50, units=1)], 100, True, True),

        # 9. User specifies a product number while checking out; checkout succeeds if the wallet is sufficient.
        (['y', '1'], [Product(name="P1", price=30, units=1)], 50, True, True),

        # 10. User removes all items from the cart and cannot checkout.
        (['n', 'y', '1', 'y'], [Product(name="P1", price=40, units=1)], 100, False, False),
    ]
)

def test_check_cart_valid_inputs(
    mocker, mock_user_input, inputs, cart_items, wallet_balance, expected_checkout_called, expected_result
):
    """
    Tests valid input scenarios for the `check_cart` function.
    """
    # Mock checkout function to simulate wallet checks
    def mock_checkout_logic(user, cart):
        total_price = sum(item.price * item.units for item in cart.retrieve_items())
        return user.wallet >= total_price

    mock_checkout = mocker.patch("online_shopping_cart.checkout.checkout_process.checkout")
    mock_checkout.side_effect = mock_checkout_logic

    # Mock external dependencies
    mocker.patch("online_shopping_cart.checkout.checkout_process.display_cart_items")
    mocker.patch("online_shopping_cart.checkout.checkout_process.global_products", [])

    # Setup the cart and user
    cart = ShoppingCart()
    for item in cart_items:
        cart.add_item(item)

    user = User(name="test_user", wallet=wallet_balance)

    # Simulate user inputs
    mock_user_input(inputs)

    # Run the function
    result = check_cart(user=user, cart=cart)

    # Validate checkout calls based on expectations
    if expected_checkout_called:
        mock_checkout.assert_called_once_with(user=user, cart=cart)
    else:
        mock_checkout.assert_not_called()

    # Validate the return value
    assert result == expected_result





#####################################################
######### Invalid Parameter Types for User/Cart #####
#####################################################

@pytest.mark.parametrize("user, cart", [
    (None, None),                  # Both inputs are None.
    ("InvalidUser", None),         # Invalid user type (string), no cart.
    (None, "InvalidCart"),         # No user, invalid cart type (string).
    (User(name="valid", wallet=0), ShoppingCart()),  # Valid user, empty cart.
    (User(name="valid", wallet=10), "InvalidCart"),  # Valid user, invalid cart type.
    (123, ShoppingCart()),         # Invalid user type (int), valid cart.
    (3.14, ShoppingCart()),        # Invalid user type (float).
    ([], ShoppingCart()),          # Invalid user type (list).
    ({}, ShoppingCart()),          # Invalid user type (dict).
    (User(name="valid", wallet=10), []),  # Valid user, invalid cart type (list).
    (User(name="valid", wallet=10), {}),  # Valid user, invalid cart type (dict).
])


def test_check_cart_invalid_user_cart(user, cart):
    """
    Tests invalid parameter scenarios for the `check_cart` function.
    Ensures that the function gracefully handles invalid inputs.
    """
    result = None
    try:
        result = check_cart(user=user, cart=cart)
    except Exception as e:
        assert isinstance(e, (TypeError, AttributeError)), f"Unexpected exception type: {type(e)}"

    # If no exception is raised, ensure result indicates failure
    assert result in [None, False], "check_cart did not handle invalid inputs properly."


#####################################################
################# Edge Case Test Cases ##############
#####################################################

@pytest.mark.parametrize("inputs, cart_items, wallet_balance, expected", [
    (['y'], [Product(name="P1", price=100, units=1)], 100, True),  # Wallet balance equals total price.
    (['y'], [Product(name="P1", price=200, units=1)], 100, False),  # Insufficient wallet balance.
    (['n', 'y', '1', 'y'], [], 100, False),  # Remove the only item and attempt to checkout.
    (['n'], [Product(name="P1", price=10, units=1)], 100, False),  # No action.
])
def test_check_cart_edge_cases(mocker, mock_user_input, inputs, cart_items, wallet_balance, expected):
    """
    Tests edge cases for the `check_cart` function.
    """
    # Mock the checkout function with dynamic behavior
    def mock_checkout_logic(user, cart):
        total_price = sum(item.price * item.units for item in cart.retrieve_items())
        return user.wallet >= total_price

    mock_checkout = mocker.patch("online_shopping_cart.checkout.checkout_process.checkout")
    mock_checkout.side_effect = mock_checkout_logic

    cart = ShoppingCart() #Cart with specified items
    for item in cart_items:
        cart.add_item(item)

    # user with the specified wallet balance
    user = User(name="test_user", wallet=wallet_balance)

    mock_user_input(inputs)  # Set the mocked user inputs
    result = check_cart(user=user, cart=cart)

    assert result == expected
