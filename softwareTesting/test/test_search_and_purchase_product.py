import pytest
from unittest.mock import patch, MagicMock
from online_shopping_cart.shop.shop_search_and_purchase import search_and_purchase_product

mock_login_info = {"username": "test_user", "wallet_balance": 100.0}


#1. Verifies that entering "all" correctly displays the entire inventory.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_all(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["all", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_csv_as_table.assert_called_once()
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#2. Tests searching for an exact product name to confirm it matches correctly.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_exact_product(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Apple", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#3. Tests partial product name searches to confirm it displays all matching results.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_partial_product(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Ap", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="ap")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#4.Verifies case-insensitive matching for product names.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_case_insensitive(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["aPpLe", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)



#5.Ensures the system can handle product names with special characters.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_with_special_characters(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Apple!@#", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple!@#")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#6.Confirms that product names with numeric characters are processed correctly.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_with_numbers(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Apple123", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple123")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#7.Tests the system's behavior when the user confirms the shopping process.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_confirm_shopping(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Apple", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)


#8.Handles the edge case where the user inputs an empty string as the search query.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_with_empty_input(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["", "y"]  # Simulate empty input
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once()  # Now this should be called instead of display_csv_as_table
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#9.Verifies that the system handles exceptions during the checkout process gracefully.
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_checkout_failure(mock_get_user_input, mock_login, mock_checkout_and_payment):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["all", "y"]

    mock_checkout_and_payment.side_effect = Exception("Checkout failed")

    with pytest.raises(Exception, match="Checkout failed"):
        search_and_purchase_product()

    mock_checkout_and_payment.assert_called_once()

# 10.Ensures that entering a non-existent product name displays no matching results.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_no_match(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["NonExistentProduct", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="nonexistentproduct")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

#11.Validates that newline characters in search input are processed appropriately.
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
def test_search_with_newline_character(mock_get_user_input, mock_login, mock_checkout_and_payment, mock_display_filtered_table, mock_display_csv_as_table):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = ["Apple\n", "y"]
    search_and_purchase_product()

    mock_login.assert_called_once()
    mock_display_filtered_table.assert_called_once_with(search_target="apple\n")
    mock_checkout_and_payment.assert_called_once_with(login_info=mock_login_info)

# 12. Simulates integer input to verify that the system raises an AttributeError.
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
def test_search_with_invalid_integer(mock_display_csv_as_table, mock_display_filtered_table, mock_checkout_and_payment,
                                     mock_login, mock_get_user_input):
    mock_login.return_value = mock_login_info
    mock_get_user_input.side_effect = [123, "y"]

    with pytest.raises(AttributeError):
        search_and_purchase_product()

    mock_get_user_input.assert_called_with(
        prompt="Search for products in inventory (type 'all' for the whole inventory): ")



# 13.  Simulates float input to verify that the system raises an AttributeError.
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
def test_search_with_invalid_integer(mock_display_csv_as_table, mock_display_filtered_table, mock_checkout_and_payment,
                                     mock_login, mock_get_user_input):
    mock_login.return_value = mock_login_info

    mock_get_user_input.side_effect = [123.4, "y"]

    with pytest.raises(AttributeError):
        search_and_purchase_product()

    mock_get_user_input.assert_called_with(
        prompt="Search for products in inventory (type 'all' for the whole inventory): ")

#14. Simulates list input to verify that the system raises an AttributeError.
@patch("online_shopping_cart.shop.shop_search_and_purchase.UserInterface.get_user_input")
@patch("online_shopping_cart.shop.shop_search_and_purchase.login")
@patch("online_shopping_cart.shop.shop_search_and_purchase.checkout_and_payment")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_filtered_table")
@patch("online_shopping_cart.shop.shop_search_and_purchase.display_csv_as_table")
def test_search_with_invalid_list(mock_display_csv_as_table, mock_display_filtered_table, mock_checkout_and_payment,
                                  mock_login, mock_get_user_input):
    mock_login.return_value = mock_login_info

    mock_get_user_input.side_effect = [['item1', 'item2'], "y"]

    with pytest.raises(AttributeError):
        search_and_purchase_product()

    mock_get_user_input.assert_called_with(
        prompt="Search for products in inventory (type 'all' for the whole inventory): ")