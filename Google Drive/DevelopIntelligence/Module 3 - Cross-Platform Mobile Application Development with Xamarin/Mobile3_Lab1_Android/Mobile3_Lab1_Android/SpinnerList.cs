
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

namespace Mobile3_Lab1_Android
{
	[Activity(Label = "SpinnerList", MainLauncher = true)]
	public class SpinnerList : Activity
	{
		protected override void OnCreate(Bundle savedInstanceState)
		{
			base.OnCreate(savedInstanceState);
			SetContentView(Resource.Layout.spinner_list);
			Spinner spinner = FindViewById<Spinner> (Resource.Id.fullNameSpinner);
			spinner.ItemSelected += new EventHandler<AdapterView.ItemSelectedEventArgs>(spinner_ItemSelected);

			// Populate a data adapter for a spinner
			string[] options = { "one", "two", "three", "four", "five" };
			ArrayAdapter adapter = new ArrayAdapter(this, Resource.Layout.text_view_for_spinner, options);

			spinner.Adapter = adapter;
		}

		private void spinner_ItemSelected(object sender, AdapterView.ItemSelectedEventArgs e)
		{
			Spinner spinner = (Spinner)sender;

			var toastText = string.Format("Selection: {0}", spinner.GetItemAtPosition(e.Position));
			Toast.MakeText(this, toastText, ToastLength.Long).Show();
		}
}
}
