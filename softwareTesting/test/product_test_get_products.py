from online_shopping_cart.product.product_data import get_products, Product
from unittest.mock import patch
import pytest

#Mock invalid CSV data
mock_csv_data_missing_columns = [
    {'Product': 'Item1', 'Price': '12.5'},  # Missing one key
    {'Product': 'Item2'},                  # Missing teo keys
    {}                                       #Missing all three keys
]

mock_csv_data_negative_values = [
    {'Product': 'Item1', 'Price': '-10', 'Units': '10'},  # Negative price
    {'Product': 'Item2', 'Price': '10', 'Units': '-5'},    # Negative units
    {'Product': 'Item3', 'Price': '-10', 'Units': '-5'}    # Negative price and units
]

mock_csv_data_boundary_conditions = [
    {'Product': 'Item1', 'Price': '0', 'Units': '0'},       # Price and units are zero
    {'Product': 'Item2', 'Price': '1e-50', 'Units': '1'},  # Underflow: Extremely close to zero
    {'Product': 'Item3', 'Price': '1e50', 'Units': '10'},  # Overflow: Extremely large price
    {'Product': 'Item4', 'Price': '-1e50', 'Units': '5'}   # Overflow: Extremely large negative price
]


mock_csv_data_special_characters = [
    {'Product': 'Item@#!', 'Price': '12.5', 'Units': '10'}, # Special characters in product name
    {'Product': '   ', 'Price': '5.0', 'Units': '2'}        # Empty product name
]

mock_csv_data_invalid_units = [
    {'Product': 'Item1', 'Price': '10', 'Units': '0.555555555'},  # Units is a float
    {'Product': 'Item2', 'Price': '15', 'Units': 'abc'},   # Units is a string
    {'Product': 'Item3', 'Price': '20', 'Units': ''}       # Units is empty
]

# Define multiple valid CSV data sets
valid_csv_data_cases = [
    # Test case 1: Single product with standard values
    [{'Product': 'Item1', 'Price': '10.0', 'Units': '5'}],

    # Test case 2: Multiple products
    [
        {'Product': 'Item1', 'Price': '10.0', 'Units': '5'},
        {'Product': 'Item2', 'Price': '20.0', 'Units': '10'},
        {'Product': 'Item3', 'Price': '15.5', 'Units': '7'}
    ],

    # Test case 3: Product names with special characters
    [{'Product': 'Item@#$', 'Price': '12.5', 'Units': '10'}],

    # Test case 4: Product names with Unicode characters
    [{'Product': '商品', 'Price': '15.0', 'Units': '7'}],

    # Test case 5: Zero price and units (assuming zero is valid)
    [{'Product': 'FreeItem', 'Price': '0.0', 'Units': '0'}],

    # Test case 6: Large price and units
    [{'Product': 'ExpensiveItem', 'Price': '1000000.0', 'Units': '100000'}],

    # Test case 7: Product with a long name
    [{'Product': 'A very long product name that exceeds normal length', 'Price': '10.0', 'Units': '5'}],

    # Test case 8: Additional columns in CSV data
    [{'Product': 'Item1', 'Price': '10.0', 'Units': '5', 'Description': 'A test product'}],

    # Test case 9: Reordered columns
    [{'Units': '5', 'Price': '10.0', 'Product': 'Item1'}],

    # Test case 10: Whitespace in product names
    [{'Product': '  Item with spaces  ', 'Price': '10.0', 'Units': '5'}],
]



#############################################
################Test valid cases#############
#############################################


@pytest.mark.parametrize("mock_csv_data", valid_csv_data_cases)
def test_get_products_valid_data(mock_csv_data):
    """Test get_products with various valid CSV data."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data):
        products = get_products()
        assert len(products) == len(mock_csv_data)
        for i, product_data in enumerate(mock_csv_data):
            assert isinstance(products[i], Product)
            assert products[i].name == product_data['Product']
            assert products[i].price == float(product_data['Price'])
            assert products[i].units == int(product_data['Units'])




#############################################
#############Test invalid cases#############
#############################################

# Case2: Test for file not found (valid input, file not found)
def test_get_products_file_not_found():
    """Test when the file does not exist."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', side_effect=FileNotFoundError):
        with pytest.raises(FileNotFoundError):
            get_products(file_name="nonexistent.csv")

#Case3: test for invalid format
def test_get_products_invalid_file_format():
    """Test with a non-CSV file format."""
    file_name_invalid_format = "products.json"
    with patch('online_shopping_cart.product.product_data.get_csv_data', side_effect=ValueError("Unsupported file format")):
        with pytest.raises(ValueError, match="Unsupported file format"):
            get_products(file_name=file_name_invalid_format)


#Case4: test for empty filename input (invalid input ,file not found)
def test_get_products_empty_file_name():
    """Test with an empty string as the file name."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', side_effect=FileNotFoundError):
        with pytest.raises(FileNotFoundError):
            get_products(file_name="")


#Case5: test empty csv file
def test_get_products_empty_file():
    """Test with an empty CSV file."""
    mock_csv_data_empty = []
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_empty):
        products = get_products()
        assert products == []  # Expect an empty list

#Case6: invalid data type
@pytest.mark.parametrize("invalid_input", [100, 100.0,None, []])
def test_get_products_invalid_input_type(invalid_input):
    """Test invalid input types."""
    # Mock get_csv_data to ensure it is not called when input is invalid
    with patch('online_shopping_cart.product.product_data.get_csv_data') as mock_csv_data:
        mock_csv_data.side_effect = OSError
        # Check if OSError is raised as expected
        with pytest.raises(OSError):
            get_products(file_name=invalid_input)

#Case7 : check invalid csv input
def test_get_products_missing_columns():
    """Test with CSV data missing required columns."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_missing_columns):
        with pytest.raises(KeyError):  # Expect KeyError for missing fields
            get_products()

#Case8 : check negative price and units value
def test_get_products_negative_values():
    """Test with negative price or units in CSV data."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_negative_values):
        with patch('online_shopping_cart.product.product_data.Product') as mock_product:
            def side_effect_check_negative(*args, **kwargs):
                price = kwargs.get('price', 0)
                units = kwargs.get('units', 0)
                if price < 0 or units < 0:
                    raise ValueError
                return mock_product.return_value

            mock_product.side_effect = side_effect_check_negative
            with pytest.raises(ValueError):
                get_products()


#Case9: Check underflow/overflow scenoria
def test_get_products_underflow_and_overflow_conditions():
    """Test with boundary values and extreme overflow cases."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_boundary_conditions):
        with patch('online_shopping_cart.product.product_data.Product') as mock_product:
            def side_effect_check_overflow(*args, **kwargs):
                price = kwargs.get('price', 0)
                if price > 1e12 or price < -1e12:
                    raise ValueError
                return mock_product.return_value

            mock_product.side_effect = side_effect_check_overflow
            with pytest.raises(ValueError):
                get_products()


#Case10: check special character
def test_get_products_special_characters():
    """Test with special characters or empty product names."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_special_characters):
        products = get_products()
        assert len(products) == 2
        assert products[0].name == 'Item@#!'
        assert products[1].name.strip() == ''  #product name is empty


#case11 test units's data type:
def test_get_products_invalid_units():
    """Test with non-integer Units values."""
    with patch('online_shopping_cart.product.product_data.get_csv_data', return_value=mock_csv_data_invalid_units):

        with pytest.raises(ValueError, match=r"invalid literal for int\(\) with base 10"):
            get_products()


