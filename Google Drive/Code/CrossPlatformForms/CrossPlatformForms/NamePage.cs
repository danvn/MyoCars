using System;

using Xamarin.Forms;

namespace CrossPlatformForms
{
	public class NamePage : ContentPage
	{
		public NamePage()
		{
			Content = new StackLayout
			{
				Children = {
					new Label { Text = "Hello ContentPage" }
				}
			};
		}
	}
}

