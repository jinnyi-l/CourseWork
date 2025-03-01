import pytest
from unittest.mock import patch, MagicMock
from online_shopping_cart.user.user_logout import logout
from online_shopping_cart.user.user_interface import UserInterface


# Test invalid input types for 'cart'
@pytest.mark.parametrize("cart_input", [123, 123.456, "cart_string", [1, 2, 3]])
def test_logout_invalid_cart_types(cart_input):
    with patch.object(UserInterface, 'get_user_input', return_value='y'):
        with pytest.raises(AttributeError):
            result = logout(cart_input)
            # The function is expected to raise an AttributeError due to missing methods


# Test cases with valid cart object

# Fixture to create a mock cart object
@pytest.fixture
def mock_empty_cart():
    cart = MagicMock()
    cart.is_empty.return_value = True
    cart.retrieve_items.return_value = []
    return cart


@pytest.fixture
def mock_non_empty_cart():
    cart = MagicMock()
    cart.is_empty.return_value = False
    cart.retrieve_items.return_value = ['item1', 'item2']
    return cart


# Test case 1: Empty cart, user confirms logout
def test_logout_empty_cart_user_confirms(mock_empty_cart):
    with patch.object(UserInterface, 'get_user_input', return_value='y'):
        result = logout(mock_empty_cart)
        assert result is True


# Test case 2: Empty cart, user declines logout
def test_logout_empty_cart_user_declines(mock_empty_cart):
    with patch.object(UserInterface, 'get_user_input', return_value='n'):
        result = logout(mock_empty_cart)
        assert result is False


# Test case 3: Non-empty cart, user confirms logout
def test_logout_non_empty_cart_user_confirms(mock_non_empty_cart):
    with patch.object(UserInterface, 'get_user_input', return_value='y'):
        result = logout(mock_non_empty_cart)
        assert result is True


# Test case 4: Non-empty cart, user declines logout
def test_logout_non_empty_cart_user_declines(mock_non_empty_cart):
    with patch.object(UserInterface, 'get_user_input', return_value='n'):
        result = logout(mock_non_empty_cart)
        assert result is False


# Test case 5: User inputs unexpected response
@pytest.mark.parametrize("user_input", ['maybe', '', '123', '@#!'])
def test_logout_unexpected_user_input(mock_empty_cart, user_input):
    with patch.object(UserInterface, 'get_user_input', return_value=user_input):
        result = logout(mock_empty_cart)
        # Since the function checks .lower().startswith('y'), any input not starting with 'y' results in False
        assert result is False


# Test case 6: User inputs 'yes' in different cases
@pytest.mark.parametrize("user_input", ['Y', 'Yes', 'YES', 'yeS'])
def test_logout_user_confirms_with_various_yes_inputs(mock_empty_cart, user_input):
    with patch.object(UserInterface, 'get_user_input', return_value=user_input):
        result = logout(mock_empty_cart)
        assert result is True


# Test case 7: User inputs 'no' in different cases
@pytest.mark.parametrize("user_input", ['N', 'No', 'NO', 'nO'])
def test_logout_user_declines_with_various_no_inputs(mock_empty_cart, user_input):
    with patch.object(UserInterface, 'get_user_input', return_value=user_input):
        result = logout(mock_empty_cart)
        assert result is False


# Test case 8: User inputs 'q' to quit
def test_logout_user_inputs_q(mock_empty_cart):
    with patch.object(UserInterface, 'get_user_input', return_value='q'):
        result = logout(mock_empty_cart)
        # Since 'q' does not start with 'y', the function should return False
        assert result is False


# Test case 9: Cart methods raise exceptions
def test_logout_cart_methods_raise_exception():
    cart = MagicMock()
    cart.is_empty.side_effect = Exception("Cart error")
    with patch.object(UserInterface, 'get_user_input', return_value='y'), \
            pytest.raises(Exception) as exc_info:
        logout(cart)
    assert "Cart error" in str(exc_info.value)


# Test case 10: Non-empty cart with special items
def test_logout_non_empty_cart_special_items():
    cart = MagicMock()
    cart.is_empty.return_value = False
    cart.retrieve_items.return_value = ['item@#!$', '商品']
    with patch.object(UserInterface, 'get_user_input', return_value='y'):
        result = logout(cart)
        assert result is True
