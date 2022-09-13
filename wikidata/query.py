import requests

url = r'https://query.wikidata.org/sparql'
country_query_file = r'country-query.txt'
city_query_file = r'city-query.txt'
path_to_data_folder = r'../data/'

def read_and_query(path):
  with open(path, 'r') as f:
    query = ''.join(f.readlines())
    r = requests.get(url, params = {'format': 'json', 'query': query})
    json = r.json()
    labels = json['head']['vars']
    bindings = json['results']['bindings']
    values = [['"' + obj[label]['value'] + '"' for label in labels] for obj in bindings]
    return labels, values

def remove_Label(labels):
  return [label.replace('Label', '') for label in labels]

def to_csv(path, labels, values):
  data = [labels] + values
  with open(path, 'w') as f:
    for row in data:
      f.write(','.join(row) + '\n')

country_labels, countries = read_and_query(country_query_file)
country_labels = remove_Label(country_labels)
country_labels[0] = 'name'

to_csv(path_to_data_folder + 'country-list.csv', country_labels, countries)

city_labels, cities = read_and_query(city_query_file)
city_labels = remove_Label(city_labels)
city_labels[0] = 'name'

to_csv(path_to_data_folder + 'city-list.csv', city_labels, cities)
