from online_shopping_cart.product.product import Product
from csv import DictReader, reader
import os
##########################
# PRODUCT DATA CONSTANTS #
##########################


#PRODUCTS_FILE_PATHNAME: str = './files/products.csv'
# 获取项目根目录的绝对路径
BASE_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# 构造 files/products.csv 的绝对路径
PRODUCTS_FILE_PATHNAME = os.path.join(BASE_DIR, 'files', 'products.csv')

##########################
# PRODUCT DATA FUNCTIONS #
##########################


def get_csv_data(csv_file_name=PRODUCTS_FILE_PATHNAME, is_dict=False) -> (list[dict[str, str | float]] |
                                                                         tuple[list[str], list[reader]]):
    with open(file=csv_file_name, mode='r', newline='') as csv_file:
        if is_dict:
            return list(DictReader(csv_file))
        csv_reader: reader = reader(csv_file)
        return next(csv_reader), list(csv_reader)


def get_products(file_name=PRODUCTS_FILE_PATHNAME) -> list[Product]:
    """
    Load products from a CSV file
    """
    products: list[Product] = []
    for row in get_csv_data(csv_file_name=file_name, is_dict=True):
        products.append(Product(
            name=row['Product'],
            price=float(row['Price']),
            units=int(row['Units'])
        ))
    return products
