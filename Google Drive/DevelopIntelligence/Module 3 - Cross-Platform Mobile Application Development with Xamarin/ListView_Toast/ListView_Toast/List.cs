
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Android.App;
using Android.Content;
using Android.OS;
using Android.Runtime;
using Android.Views;
using Android.Widget;

namespace ListView
{
	[Activity(Label = "List", MainLauncher = true)]
	public class List : ListActivity
	{
		List<ListItem> listItems = new List<ListItem>
			{
				new ListItem {Title = "First", Description = "1st item"},
				new ListItem {Title = "Second", Description = "2nd item"},
				new ListItem {Title = "Third", Description = "3rd item"}
			};

		protected override void OnCreate(Bundle savedInstanceState)
		{
			base.OnCreate(savedInstanceState);



			ListAdapter = new ListItemAdapter(this, listItems);
		}

		protected override void OnListItemClick(ListView l, View v, int position, long id)
		{
			var SelectedItem = listItems[position];
			Android.Widget.Toast.MakeText(this, SelectedItem.Title, Android.Widget.ToastLength.Short).Show();
		}
	}
}
