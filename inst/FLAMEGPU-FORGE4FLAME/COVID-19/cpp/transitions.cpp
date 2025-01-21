// Keeps indexes of all places with population belonging to any age class.
vector<vector<int>> indexes_age_classes;
// Keeps the current population size for each age class with the time
vector<pair<double,double>> age_class_size;

// Keeps the mapping between a transition and its index in the data structure age_class_size
unordered_map<string,int> transition_idx;

// Contact rates for different combinations of age classes (from file)
static vector< pair< pair< int, int >, unordered_map< int, unordered_map < string, double >* >* > > contact;

// Infection latency rates for different combinations of age classes (from file)
// static unordered_map <string,double> latency;

// Probability to infect (from file)
static unordered_map<string, double> beta;

// Contact rates
static unordered_map <string, double> contact_rates;

static int c_matrix_idx = 0;

// Infection latency rates
// static unordered_map <string, double> latency_rates;;

// Daily swab rate.
static vector<vector<double>*> sw_rates;

// Number of age classes
static int clss = 3;

// Number of latency classes
static int clss_l = 3;

static double k_param;

static bool populate_data_structures = true; 

/* Read data from file and fill probabilities */
void read_double(string fname, double& d)
{
	cout << "#### " << fname << "####" << endl;
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		getline(f,line);
		d = stod(line);
		f.close();
		cout << d << endl;
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill probabilities */
void read_vector_double(string fname, vector<double>& v)
{
	cout << "#### " << fname << "####" << endl;
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		while (getline(f,line))
		{
			v.push_back(stod(line));
			cout << stod(line) << endl; 
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill a map<int,double> */
void read_map_int_double(string fname, unordered_map<int,double>& m)
{
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		int i = 1;
		cout << "#### " << fname << "####" << endl;
		while (getline(f,line))
		{
			m.insert(pair<int,double>(i, stod(line)));
			cout << i << "\t" << m[i] << endl;
			++i;
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill a map<string,double> */
void read_matrix_string_double_l(string fname, unordered_map<string,double>& m)
{
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		vector<string> sy_cls;
		size_t pos = 0, c_pos = 0, length = 0;
		cout << "#### " << fname << "####" << endl;
		for(int j = 0; j < clss_l; j++)
		{
			sy_cls.push_back("sy_s"+to_string(j));
			cout << "				 ";
			cout << "sy_s"+to_string(j);
		}
		cout << endl;
		int j = 0;
		while (getline(f,line))
		{
			int i = 0;
			pos = 0;
			c_pos = 0;
			string inf_class = "inf_a"+to_string(j);
			// read rates
			cout << " inf_a"+to_string(j) << " ";
			length = line.length();
			do
			{
				pos = line.find(',',c_pos);
				if( pos == string::npos)
					pos = length;
				m.insert(pair<string,double>(inf_class+"_"+sy_cls[i], stod(line.substr(c_pos,pos - c_pos))));
				cout << "| " << to_string(m[inf_class+"_"+sy_cls[i]]) << " ";
				c_pos = pos+1;
				++i;
			}
			while(pos != length);
			cout << endl;
			++j;
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill a map<string,double> */
void read_matrix_string_double(string fname, unordered_map<string,double>& m)
{
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		vector<string> inf_cls;
		size_t pos = 0, c_pos = 0, length = 0;
		cout << "#### " << fname << "####" << endl;
		for(int j = 0; j < clss; j++)
		{
			inf_cls.push_back("inf_a"+to_string(j));
			cout << "				 ";
			cout << "inf_a"+to_string(j);
		}
		cout << endl;
		int j = 0;
		while (getline(f,line))
		{
			int i = 0;
			pos = 0;
			c_pos = 0;
			string sus_class = "sus_a"+to_string(j);
			// read rates
			cout << " sus_a"+to_string(j) << " ";
			length = line.length();
			do
			{
				pos = line.find(',',c_pos);
				if( pos == string::npos)
					pos = length;
				m.insert(pair<string,double>(inf_cls[i] + "_" + sus_class, stod(line.substr(c_pos,pos - c_pos))));
				cout << "| " << to_string(m[inf_cls[i]+"_"+sus_class]) << " ";
				c_pos = pos+1;
				++i;
			}
			while(pos != length);
			cout << endl;
			++j;
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill a map<string,double> */
void read_vector_string_double(string fname, unordered_map<string,double>& m)
{
	// Open file
	ifstream f (fname);
	if(f.is_open())
	{
		cout << "#### " << fname << "####" << endl;
		size_t pos = 0, c_pos = 0;
		int i = 0;
		string line;
		// Read rates
		getline(f,line);
		while(pos != line.length())
		{
			// Find field separator
			pos = line.find(',',c_pos);
			// If there is no field separator, set the last position in the string as the index of the separator
			if( pos == string::npos)
				pos = line.length();
			// Insert <key, value> pair in the map
			m.insert(pair<string,double>("sus_a" + to_string(i), stod(line.substr(c_pos,pos - c_pos))));
			cout << " sus_a" << i << " " << to_string(m["sus_a" + to_string(i)]) << endl;
			// Set the initial index of the next field	
			c_pos = pos+1;
			++i;
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}

/* Read data from file and fill a map<int,map<string,double>*> */
void read_maps_int_pmap(string fname, vector< pair< pair< int, int>, unordered_map< int, unordered_map< string, double >* >* > >& m)
{
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		vector<string> inf_cls;
		for(int j = 0; j < clss; j++)
		{
			inf_cls.push_back("inf_a"+to_string(j));
		}
		cout << "#### " << fname << "####" << endl;
		while (getline(f,line))
		{
			size_t pos = 0, c_pos = 0, length = 0;
			int sy, range_b, range_e, i = 0, j = 0;
			unordered_map<string, double>* map = new unordered_map<string, double>; 
			// Read contact matrix symptom validity
			pos = line.find(',',c_pos);
			sy = stoi(line.substr(c_pos, pos - c_pos));
			c_pos = pos+1;
			// Read contact matrix validity range value
			pos = line.find(',',c_pos);
			range_b = stoi(line.substr(c_pos, pos - c_pos));
			c_pos = pos+1;
			pos = line.find(',',c_pos);
			range_e = stoi(line.substr(c_pos, pos - c_pos));
			c_pos = pos+1;
			cout << "#### Sympthom " << sy << " - epoch " << range_b << " ~ " << range_e << " ####" << endl;
			for(int k = 0; k < clss; k++)
			{
				cout << "\t\t\t\t";
				cout << "inf_a"+to_string(k);
			}
			cout << endl;
			// read rates
			length = line.length();
			do
			{
				if(i == 0)
					cout << " sus_a"+to_string(j) << " ";
				pos = line.find(',',c_pos);
				if( pos == string::npos)
					pos = length;
				map -> insert(pair<string,double>(inf_cls[i] + "_sus_a"+ to_string(j), stod(line.substr(c_pos,pos - c_pos))));
				cout << "| " << to_string((*map)[inf_cls[i] + "_sus_a" + to_string(j)]) << " ";
				c_pos = pos+1;
				++i;
				if(i%clss==0)
				{
					i=0;
					++j;
					cout << endl;
				}
			}
			while(pos != length);
			// Add elements to the main data structure
			auto iter = m.begin();
			bool found = false;
			int idx = -1;
			while(iter != m.end() && !found)
			{
				//	Check if an entry for the current time epoch already exists	
				cout << "Selected entry validity range " << iter -> first.first << " ~ " << iter -> first.second << endl;
				if(iter -> first.first == range_b && iter -> first.second == range_e)
					found = true;
				++iter;
				++idx;
			}
			// Create a new entry
			pair<int, unordered_map<string, double >* > pp = make_pair(sy, map);
			if(!found)
			{
				pair<int, int> ppp = pair<int, int>(range_b, range_e);
				unordered_map<int, unordered_map< string, double >* >* inn_map = new unordered_map<int, unordered_map< string, double >* >;
				inn_map -> insert(pp);
				pair< pair< int, int>, unordered_map<int, unordered_map< string, double >* >* > p = make_pair(ppp, inn_map);
				m.push_back(p);
				cout << "\t-- Create a new entry for " << range_b << " ~ " << range_e << endl;
			}
			// Populate an already existent entry
			else
			{
				m.at(idx).second -> insert(pp);
				cout << "\t-- Existing entry for " << range_b << " ~ " << range_e << endl;
			}
		}
		f.close();
	}
	else
	{
		std::cerr<<"\nUnable to open " << fname << ": file do not exists\n";
		exit(EXIT_FAILURE);
	}
}


void read_map_vector_pair(string fname, vector< vector< double >* >& v)
{
	ifstream f (fname);
	string line;
	if(f.is_open())
	{
		cout << "#### " << fname << "####" << endl;
		while (getline(f,line))
		{
			size_t pos = 0, c_pos = 0, length = 0;
			int i = 1;
			vector<double>* vec = new vector<double>; 
			// read rates
			length = line.length();
			do
			{
				pos = line.find(',',c_pos);
				if( pos == string::npos)
					pos = length;
				vec -> push_back(stod(line.substr(c_pos,pos - c_pos)));
				cout << to_string(i) << ": " << to_string(vec -> at(vec -> size() -1)) << " | ";
				c_pos = pos+1;
				++i;
			}
			while(pos != length);
			cout << endl;
			v.push_back(vec);
		}

	}
	
}

void init_data_structures()
{
	read_maps_int_pmap("./c_rates", contact);
	// read_matrix_string_double_l("./l_rates", latency);
	read_vector_string_double("./beta", beta);
	read_double("./k",k_param);
	// read_map_int_double("./sw_rates",sw_rates);
	read_map_vector_pair("./sw_rates", sw_rates);
	/* Initialize data structure */
	for(int i =0; i<clss; i++)
	{
		pair<double,double> p = {-1.0, 0.0};
		age_class_size.push_back(p);
		vector<int> v;
		indexes_age_classes.push_back(v);
	}
	populate_data_structures = false;
}

void fill_indexes_age_classes(string a_class, int idx, double *Value, map <string,int>& NumPlaces, const double& time)
{
	regex e (a_class);
	/* Match all that palces used for some measurements */
	regex e1 ("(c_)");
	/* Update the timestamp */
	age_class_size[idx].first = time;
	/* Search for the indexes and compute the age class size */
	for(auto it=NumPlaces.begin(); it!=NumPlaces.end(); it++)
	{
		smatch m, m1;
		regex_search(it->first,m,e);
		regex_search(it->first,m1,e1);
		if(m.size() > 0 && m1.size() == 0)
		{
			/* Populate the vector of indexes for the current age class */
			indexes_age_classes[idx].push_back(it->second);
			/* Compute the current value for the age class siaze */
			age_class_size[idx].second+=Value[it -> second];
		}
	}
}

/* It computes such value one for each timestamp. That is, if the function is called 
* multiple times at the same time instance the computation is done oly onece, then the
* value is stored to satisfy the subsequent ones.
* When the function is called for the very first time, it also fill the data structure
* coupling each place to a specific age class (see fill_indexes_age_classes).
*/
void compute_age_class_size(int idx, double *Value, map <string,int>& NumPlaces, const double& time)
{
	/* if the value for the age class size is not updated, compute it */
	if( time != age_class_size[idx].first )
	{
		/* Update the timestamp */
		age_class_size[idx].first = time;
		/* Comupute the population size for thi age class at this time */
		age_class_size[idx].second = 0;
		for (auto i = indexes_age_classes[idx].begin(); i != indexes_age_classes[idx].end(); i++)
		{
			age_class_size[idx].second+=Value[*i];
		}
	}
}

/* Finds index of the age class corresponding to the infect involved in the contact */
int age_class_size_idx(string transition, double *Value, map <string,int>& NumPlaces, const double& time)
{
	auto it = transition_idx.find(transition);
	int idx = -1;
	if( it != transition_idx.end() )
		idx = it -> second;
	else
	{
		bool found = false;
		string inf_class = "";
		do
		{
			regex e("(inf_a" + to_string(++idx) + "){1}");
			smatch m;
			found = regex_search(transition,m,e);
		}
		while(idx < clss && !found);
		if( found )
		{
			inf_class = "(a" + to_string(idx) + "){1}";
			transition_idx[transition] = idx;
			/* Populate the vector with the indexes of this age class */
			if(indexes_age_classes.size() == 0 || indexes_age_classes[idx].size() == 0)
				fill_indexes_age_classes(inf_class, idx, Value, NumPlaces, time);
			/* regex describing the name of the transitions for each age class */
		}
		else
			idx = -1;
	}
	if( idx == -1 )
		throw std::invalid_argument( "Age class not found! Please, kill me and check the method 'age_class_size_idx'" );
	else
	{	
		compute_age_class_size(idx,Value, NumPlaces, time);
	}
	return idx;
}


// Compute the transition rate according to:
// 	- age classes of the patients involved in the contact
double C(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
	if( populate_data_structures )
		init_data_structures();
	double rate=0;
	auto c = contact_rates.find(NameTrans[T]);
	if(c != contact_rates.end() && time >= contact.at(c_matrix_idx).first.first && time < contact.at(c_matrix_idx).first.second)
		// The transition has already fired and the rate is in the map 
		rate = c -> second;
	else
	{		
		// It's the first time the transition fires (or the contact matrix has changed), hence the rate has to be computed
		cout << "\t###   ###   ###   ###   ###   ###\n\t~~~\t" << NameTrans[T] << "\t~~~\n\t###   ###   ###   ###   ###   ###" << endl;
		if(time < contact.at(c_matrix_idx).first.first || time >= contact.at(c_matrix_idx).first.second)
		{
			cout << "-- Searching for the right contact matrix --" << endl;
			int idx = 0;
			bool fnd_epoch = false;
			cout << "\t-- Searching for the right time epoch --" << endl;
			contact_rates.clear();
			auto iter = contact.begin();
			while(iter != contact.end() && !fnd_epoch)
			{
				if( iter -> first.first <= time && iter -> first.second > time )
				{
					c_matrix_idx = idx;
					fnd_epoch = true;
					cout << "\t\t-- Contact matrix validity epoch " << contact.at(c_matrix_idx).first.first << " - " << contact.at(c_matrix_idx).first.second << " (Current time: " << time << ")" << endl;
				}
				++idx;
				++iter;
			}
			if(!fnd_epoch) 
			{
				throw std::invalid_argument( "C: unable to find a contact matrix for time > " + to_string(time) + "! Please, kill me and check the input parameters" );
			}
		}
		
		/* Check the age classes involved in the contact */
		cout << "\t-- Searching for the susceptible age class --" << endl;
		// Check susceptible's age class
		int idx_s_age =-1;
		bool fnd_age = false;
		do
		{
			regex e("(sus_a"+to_string(++idx_s_age)+"){1}");
			smatch m;
			if(regex_search(NameTrans[T],m,e))
			{
				cout << "\t\t-- " << NameTrans[T] << " matches sus_a" << idx_s_age << endl;
				fnd_age = true;
			}
		}
		while( idx_s_age < clss && !fnd_age);
		if(!fnd_age) 
		{
			throw std::invalid_argument( "C: transition " + NameTrans[T] + " susceptible age class not not found! Please, kill me and check the method 'C'" );
		}
		
		/* Check the age classes involved in the contact */
		cout << "\t-- Searching for the infect age class --" << endl;
		// Check susceptible's age class
		int idx_i_age =-1;
		fnd_age = false;
		do
		{
			regex e("(inf_a"+to_string(++idx_i_age)+"){1}");
			smatch m;
			if(regex_search(NameTrans[T],m,e))
			{
				cout << "\t\t-- " << NameTrans[T] << " matches inf_a" << idx_i_age << endl;
				fnd_age = true;
			}
		}
		while( idx_i_age < clss && !fnd_age);
		if(!fnd_age) 
		{
			throw std::invalid_argument( "C: transition " + NameTrans[T] + " infect age class not not found! Please, kill me and check the method 'C'" );
		}
		
		cout << "\t-- Searching for the right symptoms --" << endl;
		int index_sy = -1;
		bool fnd_sy = false;
		do
		{
			++index_sy;
			/* Check symptoms */
			regex e("(sy_s" + to_string(index_sy) +"){1}");
			smatch m;
			if(regex_search(NameTrans[T],m,e))
			{
				cout << "\t\t-- " << NameTrans[T] << " matches sy_s" << index_sy << endl;
				fnd_sy = true;
			}
		}
		while(!fnd_sy && index_sy < clss_l);
		if(!fnd_sy) 
		{
			throw std::invalid_argument( "C: unable to find a symptom to index the correct matrix! Please, kill me and check the input parameters" );
		}
		
		//bool fnd_age = false;
		cout << "-- Searching for the right entry in the selected contact matrix --" << endl;
		auto cmtx = contact.at(c_matrix_idx).second -> find(index_sy);
		if(cmtx == contact.at(c_matrix_idx).second -> end()) 
		{
			throw std::invalid_argument( "C: unable to find symptom contact matrix " + to_string(index_sy) + " in the input data! Please, kill me and check the method 'C'" );
		}
		string c_str = "inf_a" + to_string(idx_i_age) + "_sus_a" + to_string(idx_s_age);
		auto cit = cmtx -> second -> find(c_str);
		if(cit == cmtx -> second -> end()) 
		{
			throw std::invalid_argument( "C: unable to find " + c_str + " in the input data! Please, kill me and check the method 'C'" );
		}
		string b_str = "sus_a" + to_string(idx_s_age);
		auto bit = beta.find(b_str);
		if(bit == beta.end()) 
		{
			throw std::invalid_argument( "C: unable to find " + b_str + " in the input data! Please, kill me and check the method 'C'" );
		}
		rate = cit -> second * bit -> second;
		cout << "\t\t-- " << NameTrans[T] << " beta " << bit -> second << " contact " << cit -> second << endl;
		contact_rates.insert(pair<string,double>(NameTrans[T], rate));
		
	}
	double in;
	modf(time, &in);
	int idx = age_class_size_idx(NameTrans[T], Value, NumPlaces, time);
	double intensity = 1.0/age_class_size[idx].second;
	for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
	{
		intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
	}
	double N = 0.0;
	for(int c = 0; c < clss; c++)
	{
		N+=age_class_size[c].second;
	}
	double population_response = pow((1.0-Value[NumPlaces["d_w"]]/N),k_param);
	// cout << "\t\t" << NameTrans[T] << " final rate " << rate*intensity*population_response << endl; 
	return rate*intensity*population_response;
}

// Compute the transition rate according to the daily swab rate
double SW(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
	if( populate_data_structures )
		init_data_structures();
	/* Check the age classes involved in the contact */
	cout << "\t-- Searching for the susceptible age class --" << endl;
	// Check susceptible's age class
	int idx_age =-1;
	bool fnd_age = false;
	do
	{
		regex e("(inf_a"+to_string(++idx_age)+"){1}");
		smatch m;
		if(regex_search(NameTrans[T],m,e))
		{
			cout << "\t\t-- " << NameTrans[T] << " matches inf_a" << idx_age << endl;
			fnd_age = true;
		}
	}
	while( idx_age < clss && !fnd_age);
	if(!fnd_age) 
	{
		throw std::invalid_argument( "SW: transition " + NameTrans[T] + " susceptible age class not not found! Please, kill me and check the method 'C'" );
	}
	double in;
	modf(time, &in);
	int Time = (int) in; 
	double intensity = 1.0;
	for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
	{
		intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
	}
	return sw_rates.at(idx_age) -> at(Time)*intensity;
}

// Compute the transition rate according to:
// 	- age classes of the patients involved in the contact
/* double L(double *Value, map <string,int>& NumTrans, map <string,int>& NumPlaces,const vector<string> & NameTrans, const struct InfTr* Trans, const int T, const double& time)
{
if( populate_data_structures )
init_data_structures();
double rate=0;
auto l = latency_rates.find(NameTrans[T]);
if(l != latency_rates.end())
// The transition has already fired and the rate is in the map 
rate = l -> second;
else
{
// It's the first time the transition fires, hence the rate has to be computed
auto it=latency.begin();
bool found = false;
while(	it!=latency.end() && !found )
{
/ * Check the age classes involved in the contact * /
regex e (it->first);
smatch m;
if(regex_search(NameTrans[T],m,e))
{
rate=it->second;	
cout << NameTrans[T] << " rate = " << rate << endl;
found = true;
}
++it;
}
if(!found) 
{
throw std::invalid_argument( "L: transition " + NameTrans[T] + " not not found! Please, kill me and check the method 'C'" );
}
else
latency_rates.insert(pair<string,double>(NameTrans[T], rate));
}
double intensity = 1.0;
for (unsigned int k=0; k<Trans[T].InPlaces.size(); k++)
{
intensity *= pow(Value[Trans[T].InPlaces[k].Id],Trans[T].InPlaces[k].Card);
}
return rate*intensity;
}
*/
